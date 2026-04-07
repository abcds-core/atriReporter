#' @title Retrieve Variables from ABC-DS Data

#' @description
#' Extracts one or more variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.

#' @param study Character string indicating which study's ATRI EDC data to pull.
#'   Valid options are `"abcds"` and `"trcds"`.
#' @param dataset A symbol or string specifying the dataset name within the ABC-DS data repository.
#' @param codebook A symbol or string specifying the corresponding codebook to use for metadata.
#' @param variables A character vector of variables to extract from the dataset
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.
#' @param controls A boolean value that indicates whether the function should return the controls, Default is `FALSE`

#' @return
#' A data frame containing the selected variables and any applied filters (site and/or cycle).
#' If `apply_labels = TRUE`, variable labels are attached to the output as attributes.

#' @details
#' Extracts one or more variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[dplyr]{reexports}}
#' @rdname get_data
#' @export
#' @importFrom rlang as_string enexpr
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of

get_data <- function(
  study,
  dataset,
  codebook,
  variables,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  dataset <- rlang::as_string(rlang::enexpr(dataset))
  if (study == "abcds") {
    # fmt: skip
    files <- get_atri_files(abcds, s3_archive, data_lake, edc, crf_data_exclude_phi, latest)
    data <- import_atri_file(abcds, files, !!dataset)
  } else if (study == "trcds") {
    # fmt: skip
    files <- get_atri_files(trcds, s3_topic, data_pond_brain_health_report, "Clinical%20Data", crf_data)
    data <- import_atri_file(trcds, files, !!dataset)
  }

  ids <- get_ids(data)

  if (!is.null(site)) {
    data <- filter_by_site(data, site)
  }

  if (!is.null(cycle)) {
    data <- filter_by_cycle(data, cycle)
  }

  class(data) <- c(sprintf("%s_df", study), class(data))

  if (study == "abcds") {
    if (!rlang::is_empty(variables)) {
      data <- data[data$dd_field_name %in% variables, ]
    }

    # examdate and mrseqs are found in translated value instead of field value
    data <- add_translated_value(data, variables)

    data <- atri_pivot_wider(data, dataset)

    data$site_label <- gsub(
      "^(Puerto Rico University).*",
      "\\1",
      data$site_label
    )

    if (study == "abcds") {
      control_identifiers <- get_sibling_controls()
      if (controls) {
        data <- data[data$subject_label %in% control_identifiers, ]
      } else {
        data <- data[!data$subject_label %in% control_identifiers, ]
      }
    }
  }

  codebook <- rlang::as_string(rlang::enexpr(codebook))

  # Only set up for abcds right now
  if (study == "abcds" & apply_labels) {
    data <- apply_labels(data, abcds, !!codebook)
    variables <- c(variables, setdiff(colnames(data), c(ids, variables)))
  }

  return(data[, c(ids, variables)])
}
