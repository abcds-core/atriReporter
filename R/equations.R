#' Check Derived Variable Equations Against Stored Values
#'
#' @description
#' Validates derived variables in a specified task dataset by recomputing them
#' from their source equations and comparing the results against the values
#' stored in the database. Returns a summary of any discrepancies found, making
#' it useful for identifying calculation errors or data entry inconsistencies.
#'
#' @param task Unquoted name of the task dataset to validate (e.g.,
#'   \code{recall}, \code{npi}, \code{cancellation}). Must correspond to one of
#'   the supported task identifiers: \code{recall}, \code{dsmse},
#'   \code{verbal}, \code{blockwisc}, \code{tbatgs}, \code{ntgedsd},
#'   \code{dld}, \code{npi}, \code{reiss}, \code{iq}, \code{cancellation}.
#' @param equations A data frame defining the equations to validate. Must
#'   contain the following columns:
#'   \describe{
#'     \item{task}{Character; the task name the equation belongs to.}
#'     \item{variable}{Character; the name of the derived variable to validate.}
#'     \item{formulas}{Character; the equation used to compute the derived
#'       variable, expressed as an R expression (e.g.,
#'       \code{"var_a + var_b"}).}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts the relevant rows from \code{equations} for the specified
#'     \code{task}.
#'   \item Parses the formula expressions to identify all source variables
#'     required for computation, stripping numeric coefficients.
#'   \item Fetches the task data — including both source and derived variables —
#'     using the appropriate \code{get_*()} function for the task.
#'   \item Evaluates each formula against the retrieved data using
#'     \code{\link[rlang]{eval_tidy}}.
#'   \item Compares the recomputed values against the stored values and returns
#'     rows where they disagree, ignoring \code{NA} comparisons.
#' }
#'
#' The internal dispatcher \code{f()} maps each task name to its corresponding
#' data retrieval function. Tasks in the cognition group (\code{recall},
#' \code{dsmse}, \code{verbal}, \code{blockwisc}, \code{tbatgs}) are routed
#' through \code{\link{get_cognition}}; all others use their dedicated
#' \code{get_*()} wrapper.
#'
#' @return A tibble with one row per subject–variable combination where the
#'   stored value does not match the value computed from the equation. Returns
#'   an empty tibble if all values are consistent. Columns are:
#'   \describe{
#'     \item{subject_label}{Subject identifier.}
#'     \item{site_initials}{Site identifier.}
#'     \item{event_code}{Visit or event code.}
#'     \item{variable}{Name of the derived variable that failed the check.}
#'     \item{equation}{The formula used to recompute the variable.}
#'     \item{atri}{The value currently stored in the database.}
#'     \item{expected}{The value computed from the equation.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Define equations to validate
#' equations <- data.frame(
#'   task     = c("npi", "npi"),
#'   variable = c("npi_total", "npi_distress"),
#'   formulas = c("npi_a + npi_b + npi_c", "npi_d + npi_e")
#' )
#'
#' # Check NPI derived variables
#' check_equations(npi, equations)
#'
#' # Inspect failures
#' failures <- check_equations(npi, equations)
#' failures |> dplyr::count(variable, sort = TRUE)
#' }
#'
#' @seealso
#' \code{\link{get_cognition}}, \code{\link{get_npi}}, \code{\link{get_dld}},
#' \code{\link{get_reiss}}, \code{\link{get_cancellation}},
#' \code{\link{get_iq}} for the underlying data retrieval functions.
#'
#' @export

check_equations <- function(task, equations) {
  task <- as.character(rlang::enexpr(task))
  values <- paste0(equations[equations$task == task, "variable"])
  formulas <- paste0(equations[equations$task == task, "formulas"])

  variables <- formulas |>
    strsplit("\\s* \\+ | \\- \\s*") |>
    unlist() |>
    c(values) |>
    unique()

  f <- function(type, ...) {
    type <- as.character(type)
    fun <- switch(
      type,
      recall = ,
      dsmse = ,
      verbal = ,
      blockwisc = ,
      tbatgs = atriReporter::get_cognition,
      ntgedsd = atriReporter::get_ntgedsd,
      dld = atriReporter::get_dld,
      npi = atriReporter::get_npi,
      reiss = atriReporter::get_reiss,
      iq = atriReporter::get_iq,
      cancellation = atriReporter::get_cancellation,
    )
    fun(...)
  }

  if (task %in% c("recall", "dsmse", "verbal", "blockwisc", "tbatgs")) {
    data <- f(task, !!!variables, task = !!task)
  } else {
    data <- f(task, !!!variables)
  }

  data <- dplyr::mutate(data, dplyr::across(variables, ~ as.numeric(.x)))

  purrr::map2(values, formulas, .f = function(.x, .y) {
    current_values <- data[[.x]]
    expected_values <- rlang::eval_tidy(rlang::parse_expr(.y), data = data)
    checks <- abs(current_values - expected_values) < 1e-6
    identifiers <- c("subject_label", "site_initials", "event_code")
    if (any(!checks, na.rm = TRUE)) {
      indx <- which(!checks & !is.na(checks))
      result <- data[indx, identifiers]
      result$variable <- .x
      result$equation <- .y
      result$atri <- data[indx, .x, drop = TRUE]
      result$expected <- expected_values[indx]
      return(result)
    }
  }) |>
    dplyr::bind_rows()
}

#' Write Equation Check Results to Excel
#'
#' @description
#' Validates derived variables across all supported tasks using
#' \code{\link{check_equations}} and optionally writes the results to one or
#' more Excel workbooks and/or returns them as a named list. Results can be
#' organised by task, by site, or both, with each task or site written to its
#' own worksheet.
#'
#' @param outdir Character; path to the directory where the output Excel
#'   file(s) will be saved. Required when \code{writeFile = TRUE}, ignored
#'   otherwise. Default is \code{NULL}.
#' @param by_task Logical; if \code{TRUE}, organises results by task. When
#'   \code{writeFile = TRUE}, writes \code{equation_checks_by_task.xlsx} with
#'   one worksheet per task. Default is \code{TRUE}.
#' @param by_site Logical; if \code{TRUE}, organises results by site. When
#'   \code{writeFile = TRUE}, writes \code{equation_checks_by_site.xlsx} with
#'   one worksheet per site. Default is \code{FALSE}.
#' @param returnData Logical; if \code{TRUE}, returns the assessment results as
#'   a named list with one element per task. Default is \code{TRUE}.
#' @param writeFile Logical; if \code{TRUE}, writes the results to Excel
#'   file(s) in \code{outdir}. Requires \code{outdir} to be specified.
#'   Default is \code{FALSE}.
#'
#' @details
#' Equations are loaded automatically from
#' \code{system.file("extdata/equations.csv", package = "atriReporter")} and
#' do not need to be supplied by the user.
#'
#' The function runs \code{\link{check_equations}} for each of the following
#' tasks: \code{ntgedsd}, \code{dld}, \code{npi}, \code{reiss}, \code{dsmse},
#' \code{recall}, \code{iq}, \code{cancellation}, \code{verbal},
#' \code{tbatgs}, \code{blockwisc}.
#'
#' When \code{by_site = TRUE} and \code{writeFile = TRUE}, results are filtered
#' to the following sites: \code{UPITT}, \code{UCISOM}, \code{MGH},
#' \code{WASHU}, \code{UWI}, \code{UKY}, \code{NYSIBRDD}, \code{UCAM},
#' \code{KUMC}, \code{PRUCDD}.
#'
#' Note that \code{writeFile = TRUE} requires \code{outdir} to be set;
#' if \code{outdir} is \code{NULL} no files will be written regardless of
#' \code{writeFile}.
#'
#' @return If \code{returnData = TRUE}, a named list with one tibble per task,
#'   each containing rows where stored values differ from computed values (see
#'   \code{\link{check_equations}} for column details). Returns \code{NULL}
#'   invisibly if \code{returnData = FALSE}.
#'
#' @examples
#' \dontrun{
#' # Return results only (default)
#' results <- check_all_equations()
#'
#' # Write both workbooks and return results
#' results <- check_all_equations(
#'   outdir   = "output/checks",
#'   by_task  = TRUE,
#'   by_site  = TRUE,
#'   writeFile = TRUE
#' )
#'
#' # Write files only, suppress return value
#' check_all_equations(
#'   outdir      = "output/checks",
#'   writeFile   = TRUE,
#'   returnData  = FALSE
#' )
#' }
#'
#' @seealso \code{\link{check_equations}} for the underlying validation logic.
#'
#' @importFrom utils read.csv
#' @importFrom atriReporter get_demographics
#' @importFrom purrr map_dfr map walk
#' @importFrom dplyr mutate relocate right_join
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @export

check_all_equations <- function(
  outdir = NULL,
  by_task = TRUE,
  by_site = FALSE,
  returnData = TRUE,
  writeFile = FALSE
) {
  equations <- utils::read.csv(
    system.file("extdata/equations.csv", package = "atriReporter")
  )

  # fmt: skip
  sites <- c("UPITT", "UCISOM", "MGH", "WASHU", "UWI", "UKY", "NYSIBRDD", "UCAM", "KUMC", "PRUCDD")
  tasks <- unique(equations[, c("name", "task")])

  # Can merge codebook labels when there are fewer changes
  #cb <- purrr::map_dfr(tasks, ~ get_codebook(abcds, !!.x))

  # Fetch eval_date
  eval <- atriReporter::get_demographics(de_eval_date)

  id_cols <- c("subject_label", "site_initials", "event_code")

  all_assessments <-
    tasks[["task"]] |>
    purrr::map_dfr(.f = function(x) {
      check_equations(task = !!x, equations = equations) |>
        dplyr::mutate(
          task = !!x,
          name = setNames(tasks[["name"]], tasks[["task"]])[task]
        )
    }) |>
    dplyr::relocate(name, task, .before = "variable") |>
    # fmt: skip
    dplyr::right_join(x = eval[, c(id_cols, "de_eval_date")], y = _, by = id_cols)

  if (by_site) {
    assessments_by_site <-
      sites |>
      purrr::map(~ all_assessments[all_assessments$site_initials == .x, ]) |>
      `names<-`(sites)
    if (writeFile & !is.null(outdir)) {
      wb <- openxlsx::createWorkbook()
      purrr::walk(sites, .f = function(.x) {
        openxlsx::addWorksheet(wb, sheet = .x)
        openxlsx::writeData(
          wb,
          sheet = .x,
          x = assessments_by_site[[.x]]
        )
      })
      openxlsx::saveWorkbook(
        wb,
        file = file.path(outdir, "equation_checks_by_site.xlsx"),
        overwrite = TRUE
      )
    }
  }

  if (by_task & writeFile & !is.null(outdir)) {
    assessments_by_task <-
      tasks[["task"]] |>
      purrr::map(~ all_assessments[all_assessments$task == .x, ]) |>
      `names<-`(tasks[["task"]])
    wb <- openxlsx::createWorkbook()
    purrr::walk(tasks[["task"]], .f = function(.x) {
      openxlsx::addWorksheet(wb, sheet = .x)
      openxlsx::writeData(
        wb,
        sheet = .x,
        x = assessments_by_task[[.x]]
      )
    })
    openxlsx::saveWorkbook(
      wb,
      file = file.path(outdir, "equation_checks_by_task.xlsx"),
      overwrite = TRUE
    )
  }

  if (returnData) {
    if (by_task & by_site) {
      return(c(assessments_by_task, assessments_by_site))
    }
    if (by_task) {
      return(assessments_by_task)
    }
    if (by_site) {
      assessments_by_site
    }
  }
}
