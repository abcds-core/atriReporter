#' @title Retrieve ATRI API Token
#'
#' @description
#' Retrieves the API token for the current user from the R environment for a
#' specified ATRI EDC study.
#'
#' @param study The name of the study on the ATRI EDC for which the token is
#'   requested.
#'
#' @return
#' A character string containing the formatted API token suitable for making
#' calls to the ATRI EDC data lake.
#'
#' @details
#' This function looks for the API token in the user's R environment and formats
#' it for use in ATRI EDC API calls. It is intended for internal use by other
#' functions in the package that interact with ATRI EDC data.
#'
#' @rdname get_atri_token
#' @keywords internal

get_atri_token <- function(study) {
  study <- rlang::ensym(study)
  env_var <- retrieve_from_environment(study, type = "token")
  token <- Sys.getenv(env_var)
  return(sprintf("Token %s", token))
}

#' @title Retrieve ATRI Server Path
#'
#' @description
#' Retrieves the server path for a given ATRI EDC study from the user's R
#' environment and formats it by appending any additional subfolders or paths
#' provided via \code{...}.
#'
#' @param study The name of the study on the ATRI EDC for which the server path
#'   is requested.
#' @param ... Additional subfolders or path segments appended to the server path
#'   to locate specific folders or files.
#'
#' @return
#' A character string containing the fully formatted server path suitable for
#' use in ATRI EDC API calls or file imports.
#'
#' @details
#' This function retrieves the base server path for a study stored in the user's
#' R environment and concatenates any additional folder or file path components
#' provided via \code{...}. It is intended for internal use by other functions
#' that access ATRI EDC data.
#'
#' @seealso
#'  \code{\link[rlang]{ensyms}}, \code{\link[rlang]{as_string}}, \code{\link[purrr]{map}}
#'
#' @rdname get_atri_server
#' @keywords internal
#' @importFrom rlang ensyms as_string
#' @importFrom purrr map

get_atri_server <- function(study, ...) {
  study <- rlang::ensym(study)
  path <- rlang::ensyms(...)
  env_var <- retrieve_from_environment(study, type = "server")
  server <- Sys.getenv(env_var)
  url <- paste(purrr::map(path, rlang::as_string), collapse = "/")
  paste0(server, "/", url)
}

#' @title Make a GET Request to the ATRI EDC API
#'
#' @description
#' Sends a GET request to the ATRI EDC API using a formatted server URL and a
#' user token, returning the API response.
#'
#' @param server A character string containing the fully formatted server URL
#'   pointing to the folder or file location on the ATRI EDC data lake.
#' @param token A character string containing the user's API token for
#'   authenticating with the ATRI EDC API.
#'
#' @return
#' An \code{httr2} response object.
#'
#' @details
#' This function constructs and performs a GET request to the ATRI EDC API using
#' the provided server URL and authentication token. It is intended for internal
#' use by higher-level functions that retrieve study data.
#'
#' @seealso
#'  \code{\link[httr2]{request}},
#'  \code{\link[httr2]{req_headers}},
#'  \code{\link[httr2]{req_perform}}
#'
#' @rdname atri_get
#' @keywords internal
#' @import magrittr
#' @importFrom httr2 request req_headers req_perform

atri_get <- function(server, token) {
  response <- httr2::request(server) %>%
    httr2::req_headers(
      Authorization = token
    ) %>%
    httr2::req_perform()
  isXLSX <- grepl(".xlsx$", basename(server))
  return(get_atri_data(response, xlsx = isXLSX))
}


#' @title Retrieve Data from ATRI EDC API
#'
#' @description
#' Processes a response from the ATRI EDC API, checks its status, and returns
#' the contents as either a character vector (for folders/files) or a tibble
#' (for CSV/structured data).
#'
#' @param response A response object returned by an \code{httr2} request.
#' @param xlsx A boolean value indicating whether the file to be read is xlsx, Default: FALSE
#'
#' @return
#' Either a character vector containing folder or file names, or a
#' \code{\link[tibble]{tibble}} containing the parsed contents of the requested
#' file.
#'
#' @details
#' This function inspects the API response using \code{\link[httr2]{resp_status}}
#' and \code{\link[httr2]{resp_content_type}}.
#' - If the response indicates folders or files, a character vector is returned.
#' - If the response contains CSV or structured data, it is read into a tibble
#'   using \code{\link[readr]{read_csv}}.
#'
#' The function includes checks for empty responses and provides informative
#' error messages using \code{\link[cli]{cli_abort}} and \code{\link[glue]{glue}}.
#'
#' @seealso
#'  \code{\link[httr2]{resp_status}},
#'  \code{\link[httr2]{resp_body_raw}},
#'  \code{\link[httr2]{resp_content_type}},
#'  \code{\link[jsonlite]{fromJSON}},
#'  \code{\link[readr]{read_csv}},
#'  \code{\link[rlang]{is_empty}},
#'  \code{\link[cli]{cli_abort}},
#'  \code{\link[glue]{glue}}
#'
#' @rdname get_atri_data
#' @keywords internal
#' @importFrom httr2 resp_status resp_body_string resp_content_type resp_status_desc
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_csv
#' @importFrom rlang is_empty
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom readxl read_excel

get_atri_data <- function(response, xlsx = FALSE) {
  status <- httr2::resp_status(response)
  description <- httr2::resp_status_desc(response)
  type <- httr2::resp_content_type(response)
  if (status == 200) {
    if (xlsx) {
      # Write to a temp file
      tmp <- tempfile(fileext = ".xlsx")
      writeBin(httr2::resp_body_raw(response), tmp)
      # Read the Excel file
      data <- readxl::read_excel(tmp)
      invisible(file.remove(tmp))
    } else {
      response <- httr2::resp_body_string(response)
      if (type == "application/json") {
        data <- jsonlite::fromJSON(response)$data
        colnames(data) <- to_snake_case(colnames(data))
      } else if (type == "application/force-download") {
        data <- readr::read_csv(
          response,
          show_col_types = FALSE,
          guess_max = 2000,
          name_repair = ~ gsub("[.]", "_", .x)
        )
      }
    }
  } else {
    stop(status, ": ", description)
  }
  if (rlang::is_empty(data)) {
    cli::cli_abort(c(
      "x" = "No folders or files were found.",
      "i" = glue::glue("Request URL: {request$url}"),
      "!" = "Check that the path or endpoint exists."
    ))
  } else {
    return(data)
  }
}

#' @title Retrieve ATRI EDC File Links
#'
#' @description
#' Retrieves API links for folders and files stored in the ATRI EDC for a given study,
#' optionally navigating through additional subfolders.
#'
#' @param study A symbol or string identifying the study (e.g., `abcds`, `trcds`,
#'   `test_trcds`). Used to retrieve the appropriate API token and server.
#' @param topic A symbol or string identifying the ATRI storage topic
#'   (e.g., `s3_archive`, `s3_topic`, `topics`).
#' @param topic_code A symbol or string identifying the specific topic code
#'   within the ATRI system (e.g., `data_lake`, `data_pond_brain_health_report`).
#' @param ... Additional subfolders or path segments appended to the server path
#'   to locate specific files.
#' @param site A character string specifying the site code associated with the
#'   uploaded file.
#'
#' @return
#' A character vector containing API links to the folders or files stored in the ATRI EDC.
#'
#' @details
#' The \code{get_atri_files()} function constructs API links by combining the base
#' server path for a study (retrieved via \code{\link{get_atri_server}}) with any
#' additional folder or file path components provided via \code{...}. These links
#' can then be used to retrieve data from the ATRI EDC using \code{\link{import_atri_file}}.
#'
#' @seealso
#'  \code{\link[rlang]{ensym}},
#'  \code{\link[rlang]{is_symbol}},
#'  \code{\link[rlang]{as_string}},
#'  \code{\link{import_atri_file}},
#'  \code{\link{get_atri_server}}
#'
#' @rdname get_atri_files
#' @export
#' @importFrom rlang ensym is_symbol as_string

get_atri_files <- function(study, topic, topic_code, ..., site = NULL) {
  # abcds, trcds, test_trcds
  study <- rlang::ensym(study)
  # s3_archive, s3_topic, topics
  topic <- rlang::ensym(topic)
  # data_lake, data_pond_brain_health_report
  topic_code <- rlang::ensym(topic_code)
  subfolders <- rlang::ensyms(...)

  token <- get_atri_token(!!study)
  s3 <- grepl("s3", as.character(topic))

  endpoint <- if (s3) {
    quote(items)
  } else {
    if (!is.null(site)) {
      site_query <- sprintf("?site_code=%s&output_format=json", site)
    }
    as.symbol(paste0("files", site_query))
  }

  # Call with or without subfolders
  server <- if (length(subfolders) >= 1) {
    get_atri_server(!!study, !!topic, !!topic_code, !!endpoint, !!!subfolders)
  } else {
    get_atri_server(!!study, !!topic, !!topic_code, !!endpoint)
  }

  page_size <- if (!!study == "abcds") "/?pageSize=100" else "/?page_size=100"

  if (s3) {
    url <- paste0(server, page_size)
    return <- "public_api"
  } else {
    url <- server
    return <- c("label", "code")
  }

  data <- memoise_atri_get(url, token)
  return(data[, return, drop = TRUE])
}

#' Upload a file to the ATRI API
#'
#' Uploads a file to the ATRI server for a specified study, topic, and site using
#' a multipart POST request. The function retrieves the appropriate API token and
#' server endpoint based on the supplied study and topic information, then uploads
#' the file with associated metadata.
#'
#' @param study A symbol or string identifying the study (e.g., `abcds`, `trcds`,
#'   `test_trcds`). Used to retrieve the appropriate API token and server.
#' @param topic A symbol or string identifying the ATRI storage topic
#'   (e.g., `s3_archive`, `s3_topic`, `topics`).
#' @param topic_code A symbol or string identifying the specific topic code
#'   within the ATRI system (e.g., `data_lake`, `data_pond_brain_health_report`).
#' @param site A character string specifying the site code associated with the
#'   uploaded file.
#' @param label A short character string used as the file label in the ATRI system.
#' @param description A character string providing a longer description of the file.
#' @param source_file A file path to the file to be uploaded. The file is uploaded
#'   as a multipart form field. Currently assumed to be a PDF.
#'
#' @return An HTTP response object returned by \pkg{httr2} from
#'   \code{httr2::req_perform()}. This object contains the server response and
#'   status information for the upload request.
#'
#' @details
#' This function constructs a multipart POST request using \pkg{httr2}. The file
#' is uploaded using \code{curl::form_file()} and sent along with metadata fields
#' required by the ATRI API. Authentication is handled via a token retrieved by
#' \code{get_atri_token()}, and the target server endpoint is determined using
#' \code{get_atri_server()}.
#'
#' @examples
#' \dontrun{
#' post_atri_files(
#'   study = abcds,
#'   topic = s3_archive,
#'   topic_code = data_lake,
#'   site = "123",
#'   label = "Brain Health Report",
#'   description = "Participant brain health feedback report",
#'   source_file = "report.pdf"
#' )
#' }
#'
#' @seealso
#' \code{\link{get_atri_token}}, \code{\link{get_atri_server}}
#'
#' @importFrom httr2 request req_headers req_body_multipart req_perform
#' @importFrom curl form_file
#'
#' @export

post_atri_files <- function(
  study,
  topic,
  topic_code,
  site,
  label,
  description,
  source_file
) {
  # abcds, trcds, test_trcds
  study <- rlang::ensym(study)
  token <- get_atri_token(!!study)
  # s3_archive, s3_topic, topics
  topic <- rlang::ensym(topic)
  # data_lake, data_pond_brain_health_report, transfer-kansas-brain-health-report
  topic_code <- rlang::ensym(topic_code)

  # Check for existing file
  code <- tryCatch(
    {
      existing <- get_atri_files(!!study, !!topic, !!topic_code, site = site)
      indx <- which(existing$label == label)
      existing[["code"]][indx]
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (!is.null(code)) {
    server <- get_atri_server(!!study, files, !!code)
    message(server)
    body <- list(
      topic_code = as.character(topic_code),
      site_code = site,
      source_file = curl::form_file(source_file, type = "application/pdf"),
      file_code = code,
      reason_for_change = "Adding an Updated Form"
    )
  } else {
    server <- get_atri_server(!!study, !!topic, !!topic_code, files)
    message(server)
    body <- list(
      topic_code = as.character(topic_code),
      site_code = site,
      label = label,
      description = description,
      source_file = curl::form_file(source_file, type = "application/pdf")
    )
  }

  server <- sprintf("%s?site_code=%s", server, site)

  response <- httr2::request(server) %>%
    httr2::req_headers(
      Authorization = token
    ) %>%
    httr2::req_body_multipart(!!!body) |>
    httr2::req_perform()

  return(response)
}

#' @title Import a CSV File from the ATRI EDC
#'
#' @description
#' Reads a CSV file from the ATRI EDC API. The input can be provided either as:
#' \itemize{
#'   \item a folder and filename (quoted or unquoted), or
#'   \item a URL as a character vector of length one.
#' }
#'
#' @param study The name of the study on the ATRI EDC for which data is requested.
#' @param files A character vector of file paths from the ATRI EDC data lake,
#'   typically retrieved using \code{\link{get_atri_files}}. Default: \code{NULL}.
#' @param pattern Optional. A regular expression pattern to identify the
#'   appropriate CSV file within \code{files}. Default: \code{NULL}.
#' @param url Optional. A link to the specific CSV file in the ATRI EDC data lake,
#'   typically retrieved using \code{\link{get_atri_files}}. Default: \code{NULL}.
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing the contents of the requested CSV file.
#'
#' @details
#' The \code{import_atri_file()} function provides flexible access to ATRI EDC CSV
#' files. Users can specify the file using a folder and filename combination or
#' directly via a URL. The function will retrieve the file from the API and return
#' it as a tibble for downstream analysis.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve file paths from the ATRI EDC
#'   filename <- get_atri_files(study, edc, study_data, site_list)
#'
#'   # Import a CSV file using the URL
#'   data <- import_atri_file(url = filename)
#' }
#' }
#'
#' @seealso
#'  \code{\link[rlang]{ensym}},
#'  \code{\link{get_atri_files}}
#'
#' @rdname import_atri_file
#' @export
#' @importFrom rlang ensym

import_atri_file <- function(
  study,
  files = NULL,
  pattern = NULL,
  url = NULL
) {
  study <- rlang::ensym(study)
  pattern <- try(rlang::ensym(pattern), silent = TRUE)
  if (!is.null(url)) {
    server = url
  } else if (!is.null(files) & !is.null(pattern)) {
    server <- files[grepl(pattern, files)]
    if (length(server) > 1) {
      cli::cli_abort(c(
        "x" = glue::glue("{pattern} matches multiple files."),
        "i" = "Please be more specific or pass the exact API call to URL."
      ))
    }
  } else {
    cli::cli_abort(c(
      "x" = "Please provide a URL to a CSV file on ATRI.",
      "i" = "Use atri_get_files to build the URL if you do not know the path."
    ))
  }
  token <- get_atri_token(!!study)
  data <- memoise_atri_get(server, token)

  return(data)
}
