#' name_date_vote
#'
#' @description
#' A function to extract the unique combination of date, political level, and
#' voting text for a specific selection.
#'
#' @param df A `data.frame` containing voting data.
#' @param show_details An integer specifying the row index of the vote to extract.
#'
#' @return A `data.frame` with one row containing the selected vote's `Datum`,
#' `Politische Ebene`, and `Abstimmungstext`.
#' @noRd
name_date_vote <- function(df, show_details) {
  df |>
    select(Datum, `Politische Ebene`, Abstimmungstext) |>
    unique() |>
    slice(show_details)
}

#' filter_data_vote
#'
#' @description
#' A function to filter and arrange voting data for a specific selected vote.
#'
#' @param df A `data.frame` containing the full voting data.
#' @param show_details An integer specifying the row index of the vote to filter for.
#'
#' @return A df with the selected columns
#' @noRd
filter_data_vote <- function(df, show_details) {
  selected_vote <- name_date_vote(df, show_details)

  df |>
    filter(Abstimmungstext == selected_vote$Abstimmungstext) |>
    arrange(NrGebiet) |>
    select(all_of(c(
      "Datum", "Politische Ebene", "Abstimmungstext", "NrGebiet", "Gebiet",
      "Stimmberechtigte", "Ja-Stimmen", "Nein-Stimmen", "Stimmbeteiligung (in %)",
      "Ja-Anteil (in %)", "Nein-Anteil (in %)"
    )))
}
