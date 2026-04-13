#' @title Collapse a Character Vector with 'and'
#'
#' @description
#' Collapses a character vector of two or more values into a single string,
#' inserting 'and' before the last element and using an Oxford comma if necessary.
#'
#' @param x A character vector of one or more values to collapse.
#'
#' @return
#' A single character string with the elements of \code{x} separated by commas
#' and 'and' before the last element.
#'
#' @details
#' This function is useful for formatting lists in text output. For example,
#' a vector \code{c("A", "B", "C")} will be collapsed as
#' \code{"A, B, and C"}.
#'
#' @rdname collapse_and
#' @keywords internal

collapse_and <- function(x) {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    paste(x, collapse = " and ")
  } else {
    paste0(
      paste(x[-n], collapse = ", "),
      ", and ",
      x[n]
    )
  }
}

#' @title JavaScript-Style If Operator
#'
#' @description
#' A custom infix operator to replace \code{ifelse}, mimicking JavaScript's
#' ternary operator when used in combination with \code{\%:\%}.
#'
#' @param test A logical condition of length one to evaluate.
#' @param yes The value to return if \code{test} evaluates to \code{TRUE}.
#'
#' @return
#' A list containing the condition and the value to return if the condition
#' evaluates to \code{TRUE}. This is designed to be used with \code{\%:\%}
#' to specify the \code{FALSE} case.
#'
#' @details
#' This operator provides a more readable, JavaScript-style conditional
#' expression in R. Combined with \code{\%:\%}, it mimics the ternary operator
#' (\code{condition ? value_if_true : value_if_false}) found in JavaScript.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   x <- NULL
#'   is.null(x) %?% "Condition is NULL" %:% "Condition is not NULL"
#' }
#' }
#'
#' @rdname ifelsejs
#' @export

`%?%` <- function(test, yes) {
  list(test = test, yes = yes)
}


#' @title JavaScript-Style Else Operator
#'
#' @description
#' A custom infix operator that works with \code{\%?\%} to mimic JavaScript's
#' ternary operator, providing the value to return when the condition is \code{FALSE}.
#'
#' @param lhs A list produced by \code{\%?\%}, containing the test condition and
#'   the value to return if the condition evaluates to \code{TRUE}.
#' @param no The value to return if the test condition evaluates to \code{FALSE}.
#'
#' @return
#' Returns the value specified for \code{TRUE} if the condition evaluates to
#' \code{TRUE}, otherwise returns the \code{FALSE} value.
#'
#' @details
#' Combined with \code{\%?\%}, this operator mimics the ternary operator in JavaScript:
#' \code{condition ? value_if_true : value_if_false}.
#' Use \code{\%?\%} to specify the condition and true case, and \code{\%:\%}
#' to specify the false case.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   x <- NULL
#'   is.null(x) %?% "Condition is NULL" %:% "Condition is not NULL"
#' }
#' }
#'
#' @rdname ifelsejs
#' @export

`%:%` <- function(lhs, no) {
  if (lhs$test) lhs$yes else no
}


#' @title Retrieve ATRI Token or Server from Environment
#'
#' @description
#' Attempts to locate the ATRI API token or server in a user's R environment
#' based on the specified study.
#'
#' @param study The name of the study on the ATRI EDC for which data is requested.
#' @param type The type of environment variable to return. Must be either
#'   \code{"token"} or \code{"server"}. Default: \code{c("token", "server")}.
#'
#' @return
#' A character string containing the formatted name of the environment variable
#' needed to access the ATRI API.
#'
#' @details
#' This function searches the user's R environment for the ATRI token or server
#' associated with a given study. If the variable is not found, it will throw
#' an informative error using \code{cli_abort}. Useful for programmatic access
#' to ATRI API resources without hardcoding credentials.
#'
#' @seealso
#'  \code{\link[cli]{cli_abort}},
#'  \code{\link[glue]{glue}}
#'
#' @rdname retrieve_from_environment
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom glue glue

retrieve_from_environment <- function(study, type = c("token", "server")) {
  type <- match.arg(type)
  user_env <- Sys.getenv()
  names_user_env <- names(user_env)
  study_pattern <- paste0("^(?i)", study)
  type_pattern <- ifelse(type == "token", "ATRI_TOKEN", "ATRI_SERVER")
  env_var <- names_user_env[
    grepl(
      study_pattern,
      names_user_env,
      ignore.case = TRUE
    ) &
      grepl(type_pattern, names_user_env)
  ]

  if (length(env_var) == 0) {
    cli::cli_abort(c(
      "x" = "No tokens found in the user's environment.",
      "i" = glue::glue("Add {study}_{type_pattern} to your user environment"),
      "!" = "Use usethis::edit_r_environ to open your user environment to edit manually."
    ))
  }

  return(env_var)
}


#' @title Convert Strings to Snake Case
#'
#' @description
#' Converts a character string or vector of strings to \code{snake_case},
#' replacing spaces, hyphens, and other non-alphanumeric characters with underscores.
#'
#' @param x A single character string or a character vector to convert to \code{snake_case}.
#'
#' @return
#' A character string or vector with all elements converted to \code{snake_case}.
#'
#' @details
#' Useful for standardizing variable names, column names, or any text input
#' to \code{snake_case}. The function replaces spaces, punctuation, and
#' capital letters with underscores and lowercase letters.
#'
#' @examples
#' \dontrun{
#' to_snake_case(c("FirstName", "LastName")) # "first_name", "last_name"
#' }
#' @rdname to_snake_case

to_snake_case <- function(x) {
  tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", x, perl = TRUE))
}


#' @title Split Factor Labels into Binary Columns
#'
#' @description
#' Converts a variable containing multiple coded values separated by a delimiter
#' into separate binary columns for each level using a dictionary.
#'
#' @param data A data frame containing the variable to split.
#' @param variable The variable in \code{data} to split (unquoted).
#' @param dictionary A list containing the levels and labels for the variable, Default: NULL.
#' @param delim A character string used to separate multiple values in the variable.
#'   Default: \code{"|"}.
#'
#' @return
#' A data frame with the original data and additional columns for each level of
#' the split factor, coded as 0/1.
#'
#' @details
#' This function is useful for handling variables that allow multiple selections
#' (e.g., race, comorbidities). It splits the values, converts them to factors
#' according to a dictionary, creates binary indicator columns, and joins them
#' back to the original data.
#'
#' @examples
#' \dontrun{
#' split_factor_labels(data = mydata, dictionary = mydict, variable = de_race)
#' }
#'
#' @rdname split_factor_labels
#' @keywords internal

split_factor_labels <- function(
  data,
  variable,
  dictionary = NULL,
  delim = "|"
) {
  variable <- as.character(rlang::ensym(variable))
  ids <- get_ids(data)

  original_data <- data

  data <- tidyr::separate_longer_delim(
    data[, c(ids, variable)],
    cols = variable,
    delim = delim
  )

  if (!is.null(dictionary)) {
    data[[variable]] <- factor(
      data[[variable]],
      levels = dictionary[[variable]]$levels,
      labels = trimws(dictionary[[variable]]$labels)
    )
  }

  data$value <- 1L

  data <- tidyr::pivot_wider(
    data[!is.na(data[[variable]]), ],
    id_cols = ids,
    names_from = variable,
    values_fill = 0
  )

  original_data <-
    data %>%
    dplyr::group_by(dplyr::across(ids)) %>%
    dplyr::summarise_all(sum) %>%
    atri_join(x = original_data, y = ., by = ids, join_type = full_join)

  return(original_data)
}

#' @title Identify ID Columns
#'
#' @description
#' Returns a vector of column names commonly used as identifiers in ABC-DS datasets.
#'
#' @param data A data frame.
#'
#' @return A character vector of column names that are used as identifiers
#'   (e.g., subject_label, site columns, event_label, or event_code).
#'
#' @details
#' This helper function identifies the key ID columns in a dataset to be used
#' for grouping, merging, or pivoting operations.
#'
#' @examples
#' \dontrun{
#' get_ids(mydata)
#' }
#'
#' @rdname get_ids
#' @keywords internal

get_ids <- function(data) {
  n <- colnames(data)
  indx <- grepl("subject_label|site_|event_code|event_label", n)
  n[indx]
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname format_quarto
#' @keywords internal
#' @importFrom glue glue
#' @export

format_quarto <- function(x = NULL, type, width = 100) {
  if (type == "section") {
    cat(glue::glue("\\section{{{x}}}"), sep = "\n")
  } else if (type == "subsection") {
    cat(glue::glue("\\subsection{{{x}}}"), sep = "\n")
  } else if (type == "subsubsection") {
    cat(glue::glue("\\subsubsection{{{x}}}"), sep = "\n")
  } else if (type == "newpage") {
    cat("\\newpage")
  } else if (type == "input") {
    cat(glue::glue("\\input{{{x}}}"), sep = "\n")
  } else if (type == "strwrap") {
    cat(strwrap(x, width = width), fill = TRUE)
    cat("\n\n")
  }
}

#' @title Retrieve the identifiers for the sibling controls
#' @description Retrieve Sibling Identifiers
#' @return A character vector containing a list of sibling identifiers
#' @details Retrieve Sibling Identifiers
#' @rdname get_sibling_controls
#' @export

get_sibling_controls <- function() {
  # fmt: skip
  files <- get_atri_files(abcds, s3_archive, data_lake, edc, crf_data_exclude_phi, latest)
  data <- import_atri_file(abcds, files, control)
  sibling_control_identifiers <- data[
    data$dd_field_name == "sibptid1",
    "subject_label",
    drop = TRUE
  ]
  return(sibling_control_identifiers)
}

#' @title Add variables to the ABC-DS dataset located in the translated value column
#' @description Add variables to the ABC-DS dataset located in the translated value column
#' @return A tibble or data frame
#' @details Add variables to the ABC-DS dataset located in the translated value column
#' @rdname add_translated_value
#' @keywords internal

add_translated_value <- function(
  data,
  variables,
  translated_variables = c("examdate", "mrseqs")
) {
  for (i in translated_variables) {
    if (i %in% variables) {
      if (!is.character(data$dd_revision_field_value)) {
        data$dd_revision_field_value <- as.character(
          data$dd_revision_field_value
        )
      }
      data[data$dd_field_name == i, "dd_revision_field_value"] <- data[
        data$dd_field_name == i,
        "dd_revision_field_translated_value"
      ]
    }
  }
  return(data)
}


#' @title Use a custom pivot wider that can handle an abcds_df class
#' @description Use a custom pivot wider that can handle an abcds_df class
#' @return A tibble or data frame
#' @details Use a custom pivot wider that can handle an abcds_df class
#' @rdname atri_pivot_wider
#' @keywords internal

atri_pivot_wider <- function(data, dataset) {
  custom_classes <- class(data)
  class(data) <- class(data)[!grepl("abcds_df|trcds_df", class(data))]
  ids <- get_ids(data)
  duplicates <- c("mrimeta", "taumeta", "bloodcoll")
  safe_max <- function(x) {
    if (length(x[!is.na(x)]) == 0) {
      return(NA_character_)
    } else {
      return(as.character(max(x[!is.na(x)])))
    }
  }

  data <- tidyr::pivot_wider(
    data,
    id_cols = dplyr::all_of(ids),
    names_from = dd_field_name,
    values_from = dd_revision_field_value,
    values_fn = if (dataset %in% duplicates) safe_max else NULL
  )

  class(data) <- custom_classes
  return(data)
}
