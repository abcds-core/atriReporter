#' @title Retrieve Blood-Related Variables from ABC-DS Data
#' @description
#' Extracts one or more blood-related variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
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
#' to streamline access to ABC-DS blood variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected blood variables for a specific site and cycle
#'   blood_data <- get_blood(
#'     done,
#'     site = "Site01",
#'     cycle = "Cycle2",
#'     apply_labels = TRUE
#'   )
#' }
#' }
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}},
#'
#' @rdname get_blood
#' @export
#' @importFrom rlang as_string enexpr ensyms

get_blood <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "bloodcoll",
    codebook = "bloodcoll",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' @title Retrieve CSF-Related Variables from ABC-DS Data
#' @description
#' Extracts one or more CSF-related variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
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
#' to streamline access to ABC-DS CSF variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected CSF variables for a specific site and cycle
#'   csf_data <- get_csf(
#'     csfdate, done,
#'     site = "Site01",
#'     cycle = "Cycle2",
#'     apply_labels = TRUE
#'   )
#' }
#' }
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}},
#'
#' @rdname get_csf
#' @export
#' @importFrom rlang as_string enexpr ensyms

get_csf <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "csfcoll",
    codebook = "csfcoll",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}
