#' create_main_reactable
#'
#' @description
#' Function to create the main reactable table to be used as an output in the
#' Shiny application.
#'
#' @param df A `data.frame` containing the data to be displayed in the reactable.
#' @param name_of_row_var A string specifying the name of the Shiny input variable
#' that will store the row index of the clicked row.
#'
#' @return A `reactable` object ready for rendering in a Shiny application.
#'
#' @details
#' The table:
#' - Displays unique entries for the columns `Datum`, `Politische Ebene`, and `Abstimmungstext`.
#' - Uses custom column formatting for improved readability.
#' - Supports pagination, highlighting, and custom styles.
#' - Triggers a Shiny input variable when a row is clicked.
#'
#' @noRd
create_main_reactable <- function(df, name_of_row_var) {
  reactable(
    df |>
      select(all_of(c(
        "Datum", "Politische Ebene", "Abstimmungstext"
      ))) |>
      unique(),
    paginationType = "simple",
    class = "table-hover",
    language = get_language_parameters(),
    columns = list(
      Datum = colDef(
        minWidth = 80,
        align = "left",
        cell = function(value) strftime(value, "%d.%m.%Y")
      ), # 12,5% width, 50px minimum
      `Politische Ebene` = colDef(
        minWidth = 100,
        align = "left"
      ), # 25% width, 100px minimum
      Abstimmungstext = colDef(
        minWidth = 225,
        align = "left"
      ) # 62,5% width, 250px minimum
    ),
    defaultPageSize = 5,
    onClick = JS(paste0("function(rowInfo, column) {
      // Selektion visuell markieren
      var rows = document.querySelectorAll('.rt-tr');
      rows.forEach(function(r) {r.style.cursor = 'pointer';r.classList.remove('rt-tr-selected'); });
      event.currentTarget.classList.add('rt-tr-selected');
    
      if (window.Shiny) {
        Shiny.setInputValue('", name_of_row_var, ":shiny.number', rowInfo.index + 1, { priority: 'event' })
      }
    }"))
  )
}
