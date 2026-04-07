#' Count occurrences by position across IDs
#'
#' This function counts how many IDs have a value of 1 at each sequential
#' position. It's useful for understanding the distribution of repeated
#' measurements or events across subjects.
#'
#' @param data A data frame containing the data to analyze
#' @param id An unquoted column name representing the ID variable to group by
#' @param ... Grouping variables to count by (e.g., site_label)
#' @param column An unquoted column name to filter and count
#' @param value The value to filter for in the column (default: 1)
#'
#' @return A data frame with:
#'   \item{variable}{The name of the column that was counted}
#'   \item{1, 2, 3, ...}{Columns representing each position, with values
#'         indicating how many IDs had an occurrence at that position}
#'
#' @examples
#' \dontrun{
#' # Count how many subjects completed a task at each time point
#' count_by(atri_cognition, ids, crtt_done)
#'
#' # Use with map to count multiple variables
#' purrr::map(
#'   c("crtt_done", "dsmse_done"),
#'   ~ count_by(atri_cognition, ids, !!rlang::sym(.x))
#' )
#'}
#' @export

count_by <- function(data, id, ..., column, value = 1) {
  data %>%
    dplyr::filter({{ column }} == value) %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::count(..., .data$position, name = "total") %>%
    tidyr::pivot_wider(
      names_from = .data$position,
      values_from = .data$total,
      values_fill = 0
    ) %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) %>%
    dplyr::mutate(variable = rlang::as_label(rlang::enquo(column)), .before = 1)
}

#' Count Multiple Variables and Combine Results
#'
#' Iterates over multiple variable names, counts occurrences by specified ID
#' columns, and combines the results into a single summary table with custom
#' labels. This is useful for creating frequency tables across multiple related
#' variables.
#'
#' @param data A data frame containing the variables to be counted.
#' @param names Character vector of column names in \code{data} to count.
#'   These are the actual variable names as they appear in the dataset.
#' @param labels Character vector of display labels corresponding to each name
#'   in \code{names}. Must be the same length as \code{names}. These labels
#'   will replace the variable names in the output.
#' @param ... Grouping variables to count by (e.g., site_label)
#' @param ids Unquoted column name(s) to group by when counting. Can be a single
#'   variable or multiple variables using \code{c()}.
#' @param value The value to filter for in the column (default: 1)
#' @param varname Character string specifying the name for the variable column
#'   in the output. Default column name "variable" will be renamed to this value.
#'
#' @return A tibble with the following structure:
#'   \itemize{
#'     \item A column with the name specified by \code{varname} containing the
#'       labels from the \code{labels} parameter
#'     \item Columns for each unique value in the grouping variable(s), containing
#'       counts of occurrences
#'     \item All \code{NA} values in numeric columns are converted to 0
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Iterates through each variable name and its corresponding label using
#'     \code{purrr::map2()}
#'   \item For each variable, calls \code{atriReporter::count_by()} to count
#'     occurrences grouped by the specified ID column(s)
#'   \item Replaces the first column (variable identifier) with the custom label
#'   \item Combines all individual count tables into a single data frame
#'   \item Converts any \code{NA} values in numeric columns to 0 for cleaner output
#'   \item Renames the variable column to the specified \code{varname}
#' }
#'
#' This function is particularly useful for creating summary tables that show
#' counts across multiple related variables (e.g., multiple imaging sequences,
#' multiple assessment types) with human-readable labels.
#'
#' @examples
#' \dontrun{
#' # Count multiple MRI sequences by participant group
#' mri_summary <- multiple_count_by(
#'   data = imaging_data,
#'   names = c("mri_t1", "mri_flair", "mri_dti"),
#'   labels = c("T1 MPRAGE", "3D FLAIR", "DTI"),
#'   ids = subject_label,
#'   varname = "MRI_Sequence"
#' )
#'
#' # Count assessments by subject and site
#' assessment_summary <- multiple_count_by(
#'   data = assessments,
#'   names = c("cdr_done", "mmse_done", "faq_done"),
#'   labels = c("CDR", "MMSE", "FAQ"),
#'   ids = subject_label,
#'   site_label,
#'   varname = "Assessment"
#' )
#' }
#'
#' @seealso \code{\link{count_by}}
#'
#' @importFrom purrr map2
#' @importFrom rlang sym
#' @importFrom dplyr bind_rows mutate across where rename
#'
#' @export

multiple_count_by <- function(
  data,
  names,
  labels,
  ids,
  ...,
  value = 1,
  varname
) {
  purrr::map2(names, labels, .f = function(x, y) {
    counts <- count_by(
      data = data,
      id = ids,
      ...,
      column = !!rlang::sym(x),
      value = value
    )
    counts[[1]] <- y
    return(counts)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ ifelse(is.na(.x), 0, .x)
    )) %>%
    dplyr::rename(!!varname := .data$variable)
}


#' Write Event Counts to an Excel Worksheet
#'
#' This function filters a dataset for rows matching a specified value in a
#' target column, enumerates events within each subject (or other identifier),
#' reshapes the data to a wide visit-based format, and writes the result to an
#' Excel worksheet. If the Excel file or worksheet already exists, it is replaced.
#'
#' The output includes one row per identifier, with event codes spread across
#' visit columns (e.g., \code{V1}, \code{V2}, \code{V3}), and a summary row at the
#' top containing counts of non-missing events per visit.
#'
#' @param data A data frame containing event-level data.
#'
#' @param file A character string specifying the path to the Excel file
#'   (\code{.xlsx}) to create or update.
#'
#' @param sheetName A character string giving the name of the worksheet to write.
#'   If the sheet already exists, it will be removed and recreated.
#'
#' @param id A tidy-evaluated column identifying subjects or units (e.g.,
#'   \code{subject_id}). Events are counted and ordered within this identifier.
#'
#' @param ... Additional columns to retain in the output (unquoted). These are
#'   typically descriptive or grouping variables to appear alongside the ID.
#'
#' @param column A tidy-evaluated column used to filter rows (e.g., an indicator
#'   variable marking qualifying events).
#'
#' @param value The value in \code{column} that indicates an event should be
#'   included. Defaults to \code{1}.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters \code{data} to rows where \code{column == value}.
#'   \item Orders qualifying events within each \code{id} and assigns visit
#'     positions (V1, V2, ...).
#'   \item Reshapes the data to a wide format with one row per \code{id}.
#'   \item Prepends a summary row showing counts of events per visit column.
#'   \item Writes the result to an Excel worksheet using \pkg{openxlsx}.
#' }
#'
#' Identifier variables (\code{subject_label}, \code{u19_bds_id},
#' \code{u01_niad_adds_id}) are automatically filled down/up and preserved when
#' present in the input data.
#'
#' @return
#' Invisibly returns \code{NULL}. The primary side effect is writing data to an
#' Excel file.
#'
#' @seealso
#' \code{\link[openxlsx]{writeData}},
#' \code{\link[tidyr]{pivot_wider}}
#'
#' @examples
#' \dontrun{
#' write_counts_to_xlsx(
#'   data = events,
#'   file = "counts.xlsx",
#'   sheetName = "Event Counts",
#'   id = subject_id,
#'   column = event_flag,
#'   value = 1,
#'   event_date, event_type
#' )
#' }
#'
#' @importFrom rlang `:=`
#' @importFrom dplyr select all_of group_by distinct ungroup filter mutate row_number right_join
#' @importFrom tidyr fill pivot_wider
#' @importFrom openxlsx loadWorkbook createWorkbook removeWorksheet addWorksheet writeData saveWorkbook
#'
#' @export

write_counts_to_xlsx <- function(
  data,
  file,
  sheetName,
  id,
  ...,
  column,
  value = 1
) {
  `:=` <- rlang::`:=`

  idvars <- c("subject_label", "u19_bds_id", "u01_niad_adds_id")
  identifiers <- data %>%
    dplyr::select({{ id }}, dplyr::all_of(idvars)) %>%
    dplyr::group_by({{ id }}) %>%
    tidyr::fill(idvars, .direction = "downup") %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::filter({{ column }} == value) %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::mutate(
      {{ column }} := as.integer({{ column }}),
      position = dplyr::row_number(),
      event_code = as.character(.data$event_code)
    ) %>%
    dplyr::select({{ id }}, ..., {{ column }}, .data$position) %>%
    dplyr::ungroup()

  visit_columns <- paste0("V", 1:max(data$position))
  data$position <- paste0("V", data$position)

  data <- data %>%
    tidyr::pivot_wider(
      names_from = .data$position,
      values_from = .data$event_code,
      values_fill = NA
    ) %>%
    dplyr::right_join(identifiers, ., by = "ids") %>%
    {
      rbind(
        c(
          rep(NA, 5),
          colSums(
            !is.na(dplyr::select(., dplyr::all_of(visit_columns))),
            na.rm = TRUE
          )
        ),
        .
      )
    }

  if (file.exists(file)) {
    wb <- openxlsx::loadWorkbook(file)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  if (sheetName %in% names(wb)) {
    openxlsx::removeWorksheet(wb, sheet = sheetName)
  }

  openxlsx::addWorksheet(wb, sheetName = sheetName)
  openxlsx::writeData(wb, sheet = sheetName, x = data)
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  return(invisible(NULL))
}

# write_counts_to_xlsx(
#   data = atri_imaging,
#   file = "/Users/bhelsel/Desktop/abcds_imaging.xlsx",
#   sheetName = "MRI Imaging",
#   id = ids,
#   event_code,
#   column = mri_done,
#   value = 1
# )
