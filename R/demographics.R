#' @title Retrieve and Format ABC-DS Demographic Data
#'
#' @description
#' Retrieves participant demographic data from the ABC-DS study within the ATRI
#' Electronic Data Capture (EDC) system and returns a clean, labeled dataset
#' optionally filtered by site or study cycle.
#'
#' @param ... One or more unquoted variable names to select from the demographics
#' dataset.
#' @param site An optional site code or name used to filter the demographic data;
#' defaults to \code{NULL}, returning all sites.
#' @param cycle An optional study cycle identifier used to filter the data;
#' defaults to \code{NULL}, returning all cycles.
#' @param apply_labels Logical. Whether to apply variable and factor labels from
#' the ABC-DS data dictionary. Defaults to \code{FALSE}.
#' @param controls A boolean value that indicates whether the function should return the controls, Default: FALSE
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing selected demographic variables,
#' optionally filtered by site or study cycle. If \code{apply_labels = TRUE},
#' the variables will include descriptive labels and factor levels.
#'
#' @details
#' The \code{get_demographics()} function simplifies extraction of participant-level
#' demographic data for ABC-DS analyses. Users can specify particular variables of
#' interest, optionally limit results by site or cycle, and automatically attach
#' human-readable labels using \code{\link{apply_labels}} and
#' \code{\link{apply_factor_labels}}.
#'
#' If \code{age_at_visit} is among the requested variables, the function will
#' automatically compute age at visit based on the participant’s year of birth
#' and evaluation date (\code{de_eval_date}). This ensures accurate age
#' calculation even when the variable is not explicitly stored in the dataset.
#'
#' Internally, the function uses \pkg{purrr} for functional mapping and reshapes
#' the data with \pkg{tidyr} as needed for analysis-ready output.
#'
#' @seealso
#'  \code{\link[purrr]{map}},
#'  \code{\link[tibble]{as_tibble}},
#'  \code{\link[tidyr]{pivot_wider}},
#'  \code{\link{apply_labels}},
#'  \code{\link{apply_factor_labels}}
#'
#' @examples
#' \dontrun{
#' # Retrieve basic demographics for all sites
#' demo <- get_demographics(age_at_visit, de_race, de_gender, de_ethnicity)
#'
#' # Retrieve labeled demographics for a specific site and cycle
#' demo_site <- get_demographics(
#'   age_at_visit, de_race, de_gender, de_ethnicity,
#'   site = "KUMC", cycle = 2, apply_labels = TRUE
#' )
#' }
#'
#' @rdname get_demographics
#' @export
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider

get_demographics <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "ptdemog",
    codebook = "ptdemog",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' @title Calculate Age at Visit
#'
#' @description
#' Calculates participant age at the time of evaluation based on year of birth
#' and the evaluation date (\code{de_eval_date}).
#'
#' @param data A dataset exported from the ATRI API for which to add an age_at_visit variable
#' @param controls A boolean value that indicates whether the function should return the controls, Default: FALSE
#'
#' @return
#' A data frame or \code{\link[tibble]{tibble}} with participant identifiers and
#' their calculated age (in years) at the time of evaluation.
#'
#' @details
#' The \code{calculate_age_at_visit()} function computes age at the time of evaluation
#' in years using the formula:
#' \deqn{age = year\_of\_evaluation - year\_of\_birth}
#'
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Calculate age for all participants
#'   age_tbl <- get_demographics(de_eval_date) |> calculate_age_at_visit()
#'
#' }
#' }
#'
#' @importFrom tidyr fill
#' @importFrom dplyr mutate arrange
#'
#' @rdname calculate_age_at_visit
#' @export

calculate_age_at_visit <- function(data, controls = FALSE) {
  ptdoby <-
    get_demographics(ptdob, de_eval_date, controls = controls) |>
    dplyr::arrange(.data$subject_label, .data$event_code) |>
    tidyr::fill(.data$ptdob, .by = "subject_label", .direction = "down") |>
    dplyr::mutate(
      de_eval_date = as.Date(.data$de_eval_date, format = "%Y-%m-%d"),
      de_eval_year = as.numeric(format(.data$de_eval_date, "%Y")),
      age_at_visit = .data$de_eval_year - as.numeric(.data$ptdob)
    )

  ids <- get_ids(data)
  # fmt: skip
  atri_join(data, ptdoby[, c(ids, "age_at_visit")], by = ids, join_type = dplyr::left_join)
}
