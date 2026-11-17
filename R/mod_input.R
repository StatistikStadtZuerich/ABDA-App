#' input UI Function
#'
#' @description
#' A Shiny module that provides the UI for filtering voting data, including
#' text search, date range selection, and political level filtering.
#'
#' @param id A string specifying the namespace ID for the module.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_ui <- function(id) {
  ns <- NS(id)

  # add inputs
  tagList(
    # Text input to facilitate search
    sszTextInput(
      ns("search_text"),
      "Suchtext:"
    ),

    # Select Date Range
    sszDateRange(ns("date_range"), "Datum:",
      start = "1933-01-01",
      min = "1933-01-01",
      end = Sys.Date(),
      max = Sys.Date(),
      separator = icons_stzh()("calendar")
    ),


    # Select level of vote/referendum
    sszRadioButtons(ns("level_vote"),
      "Politische Ebene der Abstimmung:",
      choices = c(
        "Alle Vorlagen",
        choices_level
      ),
      selected = "Alle Vorlagen"
    )
  )
}

#' @description
#' A Shiny module that handles server-side logic for filtering voting data based
#' on user inputs. It provides a reactive output of the filtered data and tracks
#' changes to the input values.
#'
#' @param id A string specifying the namespace ID for the module.
#'
#' @return A list of reactive elements:
#'   - `filtered_data`: A reactive expression containing the filtered data.
#'   - `has_changed`: A reactive value that increments whenever an input changes.
#'   - `current_inputs`: A list of reactive expressions providing the current values
#'     of the text search, date range, and level inputs.
#' @noRd
mod_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filter main data according to inputs
    filtered_data <- reactive({
      filter_data_with_inputs(df_main, input)
    })

    # if a reactive value is needed to indicate that any of the inputs has changed, use this:
    # # update the reactive value to indicate any of the inputs has changed
    has_changed <- reactiveVal(value = 0)
    observeEvent(
      eventExpr = list(
        input$search_text,
        input$date_range,
        input$level_vote
      ),
      handlerExpr = {
        current_val <- has_changed()
        has_changed(current_val + 1)
      },
      ignoreNULL = FALSE
    )

    return(list(
      "filtered_data" = filtered_data,
      "has_changed" = has_changed,

      # return some input values for appropriate naming of download
      "current_inputs" = list(
        "text" = reactive({
          input$search_text
        }),
        "date" = reactive({
          input$date_range
        }),
        "level" = reactive({
          input$level_vote
        })
      )
    ))
  })
}

## To be copied in the UI
# mod_input_ui("input_1")

## To be copied in the server
# mod_input_server("input_1")
