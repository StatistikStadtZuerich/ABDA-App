#' filter_data_with_inputs
#'
#' @description
#' Function to filter the main data frame based on selected input values, such as date range,
#' search text, and voting level.
#'
#' @param df A `data.frame` containing the main data to be filtered.
#' @param input_values A list containing the filtering criteria:
#'   - `date_range`: A vector of two dates specifying the start and end of the date range.
#'   - `search_text`: A string used to filter rows containing this text in the `Abstimmungstext` column.
#'   - `level_vote`: A string specifying the voting level to filter, or `"Alle Vorlagen"` to include all levels.
#'
#' @return A filtered `data.frame` based on the selected input criteria.
#' @noRd
filter_data_with_inputs <- function(df, input_values) {
  filtered <- df |>
    filter(
      Datum >= input_values$date_range[1],
      Datum <= input_values$date_range[2]
    )

  # Filter: Some search term entered
  if (input_values$search_text != "") {
    filtered <- filtered |>
      filter(grepl(input_values$search_text, Abstimmungstext, ignore.case = TRUE))
  }

  # Filter the level of vote
  if (input_values$level_vote != "Alle Vorlagen") {
    filtered <- filtered |>
      filter(`Politische Ebene` %in% input_values$level_vote)
  }

  filtered
}
