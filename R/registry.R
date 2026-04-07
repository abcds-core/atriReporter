#' @title Retrieve Registry Variables from ABC-DS Data
#' @description
#' Extracts one or more registry variables (e.g., examdate) from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param study Character string indicating which study's ATRI EDC data to pull.
#'   Valid options are `"abcds"` and `"trcds"`.
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.
#' @param controls A boolean value that indicates whether the function should return the controls, Default is `FALSE`
#'
#' @return
#' A data frame containing the selected variables and any applied filters (site and/or cycle).
#' If `apply_labels = TRUE`, variable labels are attached to the output as attributes.
#'
#' @details
#' This function provides a convenient wrapper around get_data
#' to streamline access to ABC-DS registry variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected health variables for a specific site and cycle
#'   registry_data <- get_registry(
#'     examdate,
#'     apply_labels = TRUE
#'   )
#' }
#' }
#'
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}},
#'
#' @rdname get_registry
#' @export
#' @importFrom rlang as_string enexpr ensyms

get_registry <- function(
  ...,
  study = c("abcds", "trcds"),
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  study <- match.arg(study)
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = study,
    dataset = "registry",
    codebook = "registry",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' Create a registry summary table
#'
#' This function generates a summarized enrollment table from the ATRI registry,
#' counting participants by year and event, and appending a total row at the bottom.
#' The resulting table is formatted as a `flextable` with a custom ATRI theme.
#'
#' @return A `flextable` object summarizing enrollment by year and event.
#'
#' @importFrom dplyr mutate if_else count summarise across where bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom flextable flextable
#' @export

create_registry_table <- function() {
  registry <- atriReporter::get_registry(examdate, apply_labels = TRUE)
  registry$event_label <- gsub(" - Month [0-9]{1,2}", "", registry$event_label)

  enrollment_tbl <- registry |>
    dplyr::mutate(
      Year = dplyr::if_else(
        !is.na(examdate),
        format(as.Date(examdate), "%Y"),
        "Unknown Date"
      )
    ) |>
    dplyr::count(Year, event_label) |>
    tidyr::pivot_wider(
      names_from = event_label,
      values_from = n,
      values_fill = 0
    )

  enrollment_tbl |>
    dplyr::bind_rows(
      enrollment_tbl |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x))) |>
        dplyr::mutate(Year = "Totals", .before = 1)
    ) |>
    flextable::flextable() |>
    atriReporter:::ft_add_abcds_theme()
}
