#' get_second_reactable
#'
#' @description
#' Function to prepare data and create a secondary, expandable reactable for
#' detailed information about voting results.
#'
#' @param filtered_data A `data.frame` containing filtered voting data to be displayed.
#' @param name_vote A string representing the name of the vote for contextual reference.
#'
#' @return A `reactable` object with expandable rows for detailed data.
#'
#' @details
#' The `get_second_reactable` function:
#' - Formats and styles the voting data for better readability.
#' - Adds expandable rows to show detailed information per region.
#' - Includes custom bar charts for visualizing percentage data.
#' - Ensures consistent column alignment and formatting for a polished output.
#'
#' **Key Features:**
#' - Expandable rows with detailed tables for each region.
#' - Customizable bar charts for percentage representation.
#' - Pagination and styled headers for better navigation and aesthetics.
#' @noRd
get_second_reactable <- function(filtered_data, name_vote) {
  # always have one decimal
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

  # Prepare dfs
  data_vote <- filtered_data |>
    mutate(
      Chart_Anteil = specify_decimal(`Ja-Anteil (in %)`, 1)
    ) |>
    arrange(NrGebiet) |>
    select(all_of(c(
      "Gebiet", "Stimmbeteiligung (in %)", "Ja-Anteil (in %)", "Chart_Anteil",
      "Nein-Anteil (in %)"
    )))

  data_detail <- filtered_data |>
    select(all_of(c(
      "Gebiet", "Stimmberechtigte", "Ja-Stimmen", "Nein-Stimmen"
    ))) |>
    pivot_longer(!Gebiet) |>
    # When row is empty or 0 (mainly Stimmberechtigt is empty or 0 because of old data) then delete
    filter(!is.na(value) & value != 0) |>
    mutate(dummy_links = " ",
           dummy_rechts = " ") |> 
    select(dummy_links, everything())

  reactable(data_vote,
    paginationType = "simple",
    class = "table-hover",
    language = get_language_parameters(),
    columns = list(
      Gebiet = colDef(
        minWidth = 40,
        sortable = FALSE
      ),
      `Stimmbeteiligung (in %)` = colDef(
        html = TRUE,
        name = "Beteiligung<br>(in %)",
        minWidth = 30,
        align = "left",
        cell = function(value) {
          if (!is.na(value)) {
            return(specify_decimal(value, 1))
          } else {
            "–"
          }
        }
      ),
      `Ja-Anteil (in %)` = colDef(
        minWidth = 20,
        html = TRUE,
        name = "Ja-Anteil<br>(in %)",
        align = "right",
        headerClass = "barHeadershares hide-mobile",
        class = "hide-mobile"
      ),
      Chart_Anteil = colDef(
        minWidth = 70,
        html = TRUE,
        name = "Ja-/Nein-<br>Anteil (in %)",
        align = "center",
        cell = function(value) {
          width <- paste0(value, "%")
          bar_chart(value,
            width = width,
            fill = get_zuericolors("qual6", nth = 1),
            background = get_zuericolors("div9val", nth = 5)
          )
        },
        class = "bar",
        headerClass = "barHeader"
      ),
      `Nein-Anteil (in %)` = colDef(
        minWidth = 20,
        name = "Nein-Anteil (in %)",
        align = "left",
        class = "bar hide-mobile",
        headerClass = "barHeader hide-mobile"
      )
    ),
    details = function(index) {
      det <- filter(
        data_detail,
        Gebiet == data_vote$Gebiet[index]
      ) |> select(-Gebiet)
      htmltools::div(
        class = "Details",
        reactable(det,
          class = "innerTable",
          outlined = TRUE,
          fullWidth = TRUE,
          columns = list(
            # dummy-Spalte, damit die Detail-Tabelle eingerückt erscheint
            dummy_links = colDef(
              width = 45,
              name = " ",
              align = "center",
              sortable = FALSE
            ),
            name = colDef(
              name = "Details",
              align = "left",
              minWidth = 40,
              maxWidth = 405,
              sortable = FALSE
            ),
            value = colDef(
              name = "Wert",
              align = "center",
              minWidth = 30,
              maxWidth = 316,
              sortable = FALSE,
              cell = function(value) {
                if (is.numeric(value)) {
                  format(value, big.mark = " ")
                } else {
                  return(value)
                }
              }
            ),
            # braucht leere Spalte für die ausklappbare Tabelle, damit die graue Linie oben durchgeht bis nach rechts
            dummy_rechts = colDef(
              maxWidth = 90,
              name = " ",
              align = "center",
              sortable = FALSE
            )
          )
        )
      )
    },
    onClick = "expand",
    defaultPageSize = 13
  )
}
