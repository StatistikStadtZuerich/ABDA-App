#' select_data_download
#'
#' @description
#' A function to filter and select relevant columns from a data frame for
#' downloading voting data.
#'
#' @param df A `data.frame` containing the full voting data.
#'
#' @return A df with selected columns
#' @noRd
select_data_download <- function(df) {
  df |>
    select(all_of(c(
      "Datum", "Politische Ebene", "Abstimmungstext", "Gebiet", "Stimmberechtigte",
      "Ja-Stimmen", "Nein-Stimmen", "Stimmbeteiligung (in %)", "Ja-Anteil (in %)",
      "Nein-Anteil (in %)"
    )))
}
#' prep_filename_download
#'
#' @description
#' A function to create a sanitized filename for downloading voting results, based on the title.
#'
#' @param title A character string representing the title to be used in the filename.
#'
#' @return A sanitized character string formatted as `"Abstimmungsresultat_<title>"`.
#' Spaces are replaced with dashes (`-`), and punctuation is removed.
#' @noRd
prep_filename_download <- function(title) {
  paste0(
    "Abstimmungsresultat_",
    title |>
      stringr::str_replace_all(" ", "-") |>
      stringr::str_replace_all("[:punct:]", "")
  )
}
