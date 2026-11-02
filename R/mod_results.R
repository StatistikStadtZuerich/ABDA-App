#' results UI Function
#'
#' @description
#' A Shiny module to display a filtered list of voting results and allow
#' users to select a specific vote for detailed analysis.
#'
#' @param id A string specifying the namespace ID for the module.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Title for table
    h1("Die untenstehenden Vorlagen entsprechen Ihren Suchkriterien"),
    
    # Define subtitle
    p(paste0(
      "Die Ergebnisse der ausgewählten Vorlagen sind beim Herunterladen ersichtlich. ",
      "Für Detailinformationen zur Stimmbeteiligung nach Stadtkreis und zum Ergebnis ",
      "einer Abstimmung wählen Sie eine Zeile aus."
    )),

    # Table Output to select vote
    shinycssloaders::withSpinner(
      reactableOutput(ns("vote_list")),
      type = 7,
      color = "#0F05A0"
    ),

    # if a row of the reactable is selected show details
    conditionalPanel(
      "input.show_details > 0",
      ns = ns,
      mod_details_ui(ns("details_1"))
    ),

    # initialise hidden variable for row selection, to be used with JS function in reactable
    conditionalPanel(
      "false",
      numericInput(
        label = NULL,
        inputId = ns("show_details"),
        value = 0
      )
    )
  )
}

#' results Server Functions
#' @description
#' A Shiny module to handle server-side logic for displaying voting results,
#' allowing row selection, and passing detailed data to other modules.
#'
#' @param id A string specifying the namespace ID for the module.
#' @param filtered_data A reactive expression providing a filtered data frame of voting results.
#' @param actionbutton A reactive trigger, typically tied to a user action like a button press.
#' @param current_input1 A reactive expression providing additional filtering or context information.
#' @param current_input2 A reactive expression providing additional filtering or context information.
#'
#' @return A list of reactive elements:
#'   - `data_download`: A reactive data frame prepared for downloading.
#'   - `title_excel`: A reactive string for naming the downloaded file.
#'   - `show_details`: A reactive numeric input for tracking selected rows.
#' @noRd
mod_results_server <- function(id, filtered_data, actionbutton, current_input1, current_input2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # main output reactive to actionbutton
    main_output <- reactive(
      create_main_reactable(filtered_data(), ns("show_details"))
    ) |>
      bindEvent(actionbutton())

    # main Reactable Output
    output$vote_list <- renderReactable({
      main_output()
    })

    # update the show_details to zero when any of the inputs are changed
    observeEvent(
      actionbutton(),
      updateNumericInput(session, "show_details", value = 0),
      ignoreNULL = FALSE
    )

    # create reactive with info about selected vote
    data_vote <- reactive({
      req(input$show_details > 0)
      filter_data_vote(filtered_data(), input$show_details)
    }) |>
      bindEvent(input$show_details)

    name_vote <- reactive({
      req(input$show_details > 0)
      name_date_vote(filtered_data(), input$show_details)
    }) |>
      bindEvent(input$show_details)

    # prepare strings (filename, selection string for excel)
    title_excel <- reactive({
      if (input$show_details > 0) {
        paste0(name_vote()$Abstimmungstext)
      } else {
        paste0(
          current_input1(), " ", current_input2()
        )
      }
    }) |>
      bindEvent(input$show_details, actionbutton())


    data_download <- reactive({
      if (input$show_details > 0) {
        select_data_download(data_vote())
      } else {
        select_data_download(filtered_data()) |>
          filter(Gebiet == "Stadt Zürich")
      }
    }) |>
      bindEvent(input$show_details, actionbutton())

    # call details module
    observe(
      x = {
        req(input$show_details > 0)
        mod_details_server(
          "details_1",
          data_vote,
          name_vote
        )
      }
    ) |>
      bindEvent(input$show_details)


    return(list(
      "data_download" = data_download,
      "title_excel" = title_excel,
      "show_details" = reactive({
        input$show_details
      })
    ))
  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
