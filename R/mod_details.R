#' details UI Function
#'
#' @description
#' A Shiny module to display detailed information about a selected vote, including
#' the title and a detailed table with additional information.
#'
#' @param id A string specifying the namespace ID for the module.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_details_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Name of selected vote
    htmlOutput(ns("title_vote")),

    # table with info about selected candidate - requires show_details > 0
    shinycssloaders::withSpinner(
      reactableOutput(ns("selected_vote")),
      type = 7,
      color = "#0F05A0"
    )
  )
}

#' details Server Functions
#'
#' @description
#' A Shiny server function that contains the server-side logic for the module displaying detailed information about a selected vote.
#' It renders the title of the vote and displays a detailed table with additional information about the selected vote.
#'
#' @param id A string specifying the namespace ID for the module.
#' @param data_vote A reactive object containing the data of the vote.
#' @param name_vote A reactive object containing the text of the vote.
#'
#' @details
#' This function uses `moduleServer` to define the server logic for the module. It renders the vote title as HTML and a `reactable` table
#' displaying the vote details. The table is populated using the `get_second_reactable` function.
#'
#' @noRd
mod_details_server <- function(id, data_vote, name_vote) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$title_vote <- renderText({
      paste("<br><h2>", name_vote()$Abstimmungstext, "</h2><hr>")
    })

    output$selected_vote <- renderReactable({
      get_second_reactable(data_vote(), name_vote()$Abstimmungstext)
    })
  })
}

## To be copied in the UI
# mod_details_ui("details_1")

## To be copied in the server
# mod_details_server("details_1")
