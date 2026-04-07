#' Generate Demographic Projections
#'
#' Retrieves current demographic data and rescales enrollment counts to a
#' target projection size, broken down by race, ethnicity, and gender.
#'
#' @param projection_no Integer. Target total enrollment count to project onto.
#'   Current observed proportions are preserved; cell counts are rescaled to
#'   sum to this value.
#' @param controls Logical. If `TRUE`, include control subjects in the
#'   demographic pull. Passed directly to `get_demographics()` and
#'   `get_disposition()`. Default `FALSE`.
#' @param include_ltfu Logical. If `TRUE`, retain subjects lost to follow-up
#'   (LTFU) in the counts. If `FALSE` (default), LTFU subjects identified via
#'   `get_disposition()` are excluded before summarising.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`Original`}{A data frame of observed counts cross-tabulated by
#'       ethnicity, gender, and race category, with row and column totals
#'       appended.}
#'     \item{`Projected`}{The same structure with counts rescaled to
#'       `projection_no`, rounded to whole numbers.}
#'   }
#'
#' @details
#' Race categories are treated as non-exclusive in the source data; subjects
#' endorsing more than one race category are recoded into a synthetic
#' `"More than One Race"` category and removed from all individual race columns.
#'
#' Projection scaling preserves within-column proportions:
#' each cell is multiplied by `(projection_no / column_total)`. Cells whose
#' column total is zero are left as zero. Note that independent rounding of
#' each cell may cause projected column totals to differ from `projection_no`
#' by ±1; use a largest-remainder method if exact totals are required.
#'
#' @seealso [get_demographics()], [get_disposition()]
#'
#' @examples
#' \dontrun{
#' result <- get_demographic_projections(projection_no = 500)
#' result$Original
#' result$Projected
#'
#' # Include control subjects and retain LTFU
#' result_full <- get_demographic_projections(
#'   projection_no = 500,
#'   controls      = TRUE,
#'   include_ltfu  = TRUE
#' )
#' }
#' @export
#' @importFrom dplyr filter_out all_of mutate group_by summarise across where if_else
#' @importFrom tidyr pivot_longer pivot_wider

get_demographic_projections <- function(
  projection_no,
  site = NULL,
  controls = FALSE,
  include_ltfu = FALSE
) {
  demo_vars <- c("de_gender", "de_race", "de_ethnicity")

  race_categories <- c(
    "White",
    "Black/African-American/African/Caribbean/Black British",
    "Asian/Asian British",
    "American Indian/Alaskan Native",
    "Native Hawaiian/Other Pacific Islander"
  )

  # ── Pull & recode demographics ──────────────────────────────────────────────
  # fmt: skip
  demo_df <- get_demographics(!!!demo_vars, site = site, apply_labels = TRUE, controls = controls)

  # fmt: skip
  demo_df$de_ethnicity <- factor(demo_df$de_ethnicity, levels = c("No", "Yes"), labels = c("Not Hispanic/Latino", "Hispanic/Latino"))

  if (!any(demo_df$de_gender == "Other")) {
    demo_df$de_gender <- factor(demo_df$de_gender, levels = c("Female", "Male"))
  }

  # ── Optionally drop LTFU subjects ───────────────────────────────────────────
  if (!include_ltfu) {
    disp <- get_disposition(sdstatus, controls = controls)
    demo_df <- dplyr::filter_out(demo_df, subject_label %in% disp$subject_label)
  }

  # ── Ensure all expected race columns exist ───────────────────────────────────
  all_race_columns <- c(race_categories, "Other", "Unknown or Not Reported")
  missing_cols <- setdiff(all_race_columns, colnames(demo_df))

  if (length(missing_cols) > 0) {
    demo_df[, missing_cols] <- 0
  }

  demo_df[["More than One Race"]] <- as.numeric(
    rowSums(demo_df[, race_categories]) > 1
  )

  demo_df[demo_df[["More than One Race"]] == 1, race_categories] <- 0

  # ── Summarise data ───────────────────────────────────────────────────────────

  demo_tbl <- demo_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(all_race_columns, "More than One Race")),
      names_to = "Category"
    ) |>
    dplyr::mutate(
      Category = factor(
        Category,
        levels = c(all_race_columns, "More than One Race")
      )
    ) |>
    dplyr::group_by(de_ethnicity, de_gender, Category) |>
    dplyr::summarise(n = sum(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = c(de_ethnicity, de_gender),
      names_expand = TRUE,
      values_from = n,
      values_fill = 0
    )

  totals <- rowSums(demo_tbl[, sapply(demo_tbl, is.numeric)])
  newtotals <- totals / sum(totals) * projection_no

  demo_tbl_adj <- demo_tbl |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ dplyr::if_else(totals == 0, 0, round((.x / totals) * newtotals))
      )
    )

  add_totals <- function(x) {
    x$Totals <- rowSums(x[, sapply(x, is.numeric)])
    col_totals <- colSums(x[, sapply(x, is.numeric)])
    # fmt: skip
    rbind(x, cbind(Category = "Totals", as.data.frame(as.list(col_totals), check.names = FALSE)))
  }

  list(
    Original = add_totals(demo_tbl),
    Projected = add_totals(demo_tbl_adj)
  )
}

# participants <- atriReporter::get_demographic_projections(45, site = "UCAM")
# controls <- atriReporter::get_demographic_projections(
#   7,
#   site = "UCAM",
#   controls = TRUE
# )

# originals <- dplyr::bind_rows(participants$Original, controls$Original)
# projections <- dplyr::bind_rows(participants$Projected, controls$Projected)

# projections |>
#   dplyr::mutate(
#     Group = c(rep("Participants", 9), rep("Controls", 9)),
#     Category = gsub("/.*British$", "", Category),
#     # fmt: skip
#     Category = ifelse(Category == "Black", "Black or African American", Category)
#   ) |>
#   flextable::as_grouped_data(groups = "Group") |>
#   flextable::as_flextable(hide_grouplabel = TRUE) |>
#   flextable::set_header_labels(
#     values = c("", rep(c("Female", "Male"), 2), "Totals")
#   ) |>
#   flextable::add_header_row(
#     values = c("", "Not Hispanic/Latino", "Hispanic/Latino", ""),
#     colwidths = c(1, 2, 2, 1)
#   ) |>
#   atriReporter:::ft_add_abcds_theme(grouped_column = TRUE) |>
#   flextable::width(j = 1, width = 3) |>
#   flextable::width(j = 2:6, width = 1) |>
#   flextable::bold(i = ~ is.na(Category)) |>
#   flextable::padding(padding = 2) |>
#   flextable::padding(i = ~ !is.na(Category), j = 1, padding.left = 10) |>
#   flextable::save_as_docx(
#     path = "/Users/bhelsel/Desktop/projections.docx",
#     pr_section = officer::prop_section(
#       page_size = officer::page_size(
#         orient = "landscape",
#         width = 8.3,
#         height = 11.7
#       )
#     )
#   )
