#' Application server logic
#'
#' The server-side logic of the Shiny application. This function handles
#' the reactive inputs, module calls, and server logic for the application.
#'
#' @param input Internal parameter for {shiny}. Automatically passed; do not modify.
#' @param output Internal parameter for {shiny}. Automatically passed; do not modify.
#' @param session Internal parameter for {shiny}. Automatically passed; do not modify.
#'
#' @details
#' The `app_server` function integrates several modules and reactive expressions
#' to create the core server logic of the Shiny application. It includes:
#' - A filter module for processing inputs (`mod_input_server`).
#' - A results module for displaying filtered and processed data (`mod_results_server`).
#' - Observers to handle button updates, data downloads, and event bindings.
#'
#' The function also includes functionality to:
#' - Update the label of an action button dynamically.
#' - Print details for debugging or further analysis.
#' - Create and manage downloadable Excel files using preprocessed data.
#'
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  filtered_input <- mod_input_server("input_1")
  output_result <- mod_results_server(
    "results_1",
    filtered_input$filtered_data,
    reactive({
      input$action_button
    }),
    filtered_input$current_inputs$text,
    filtered_input$current_inputs$level
  )

  observe({
    # Update the Action Button
    updateActionButton(
      session,
      "action_button",
      label = "Erneute Abfrage"
    )
  }) |>
    bindEvent(input$action_button)

  observe({
    print(output_result$show_details())
  })


  observe({
    filename <- prep_filename_download(output_result$title_excel())


    mod_download_server(
      id = "download_1",
      data_download = output_result$data_download(),
      fn_no_ext = filename,
      fct_create_excel = create_excel,
      excel_args = list(
        output_result$data_download(),
        output_result$title_excel()
      )
    )
  }) |>
    bindEvent(input$action_button, output_result$show_details())
}
