#' get_language_parameters 
#'
#' @description
#' A utility function to set the language parameters for `reactable` tables,
#' specifying the text displayed for various UI elements like pagination.
#'
#' @return A `reactableLang` object containing the language settings for
#' pagination and data display in a `reactable` table.
#'
#' @return A `reactableLang` object that configures the language for pagination
#' and other UI elements in the `reactable` table.
#'
#' @noRd
get_language_parameters <-  function() {
  reactableLang(
    noData = "Keine Einträge gefunden",
    pageNumbers = "{page} von {pages}",
    pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
    pagePrevious = "\u276e",
    pageNext = "\u276f",
    pagePreviousLabel = "Vorherige Seite",
    pageNextLabel = "Nächste Seite"
  )
}
#' bar_chart
#'
#' @description
#' Function to render a bar chart within a reactable cell, with an optional label
#' displayed to the left of the chart.
#'
#' @param label The label to be shown on the left side of the chart.
#' @param width The width of the bar chart. Defaults to `"100%"`.
#' @param height The height of the bar chart. Defaults to `"2rem"`.
#' @param fill The fill color of the bar chart. Defaults to `"#00bfc4"`.
#' @param background The background color of the bar chart. Defaults to `NULL`.
#'
#' @return A `div` containing the label and the bar chart.
#' @noRd
bar_chart <- function(label, width = "100%", height = "2rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0rem", background = background), bar)
  div(style = list(display = "flex"), chart)
}