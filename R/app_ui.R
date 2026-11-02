#' The application User-Interface
#'
#' This function defines the user interface (UI) of the Shiny application. It
#' organizes the layout, panels, and interactive elements that users interact with.
#'
#' @param request Internal parameter for `{shiny}`. Automatically populated
#' by Shiny; do not modify or remove.
#'
#' @details
#' The `app_ui` function includes:
#' - A sidebar with an input module (`mod_input_ui`) and an action button.
#' - Conditional display of a download module (`mod_download_ui`) after the action button is triggered.
#' - A main panel that conditionally displays the results UI (`mod_results_ui`) based on user interaction.
#'
#' link (`ogd_link`) is provided for downloads.
#'
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  ogd_link <- "https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933"

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ssz_page(

      mod_input_ui("input_1"),
      
      tags$div(
        class = "button-div",
        sszActionButton(
          "action_button",
          "Abfrage starten"
        ),
        conditionalPanel(
          condition = "input.action_button>0",
          mod_download_ui("download_1", ogd_link)
        )
      ),
      br(),
      conditionalPanel(
        condition = "input.action_button>0",
        mod_results_ui(
          "results_1"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Abstimmungsdatenbank"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
