#' Create Excel File for Download
#'
#' @description
#' A utility function to create and write an Excel file with multiple sheets,
#' customized content, and styling.
#'
#' @param file A character string specifying the file path where the Excel file
#' should be saved.
#' @param excel_args A list of arguments:
#'   - `excel_args[[1]]`: A data frame containing the main data to be written
#'     to the second sheet.
#'   - `excel_args[[2]]`: A string used to customize the title and content of
#'     the Excel file.
#'
#' @details
#' This function creates an Excel file with:
#' - A title page including a logo and customized title.
#' - A second sheet with the provided data.
#' - Basic styling and formatting applied to improve readability.
#'
#' @noRd
create_excel <- function(file, excel_args) {
  # data_for_download <- prepare_data_for_excel(excel_args[[1]])
  data_for_download <- excel_args[[1]]
  string_choice <- excel_args[[2]]

  # Data Paths
  path_title_page <- here::here("inst", "app", "www", "Titelblatt.xlsx")
  path_logo <- here::here("inst", "app", "www", "logo_stzh_stat_sw_pos_1.png")

  # Read Data
  data <- read.xlsx(path_title_page, sheet = 1)

  # Manipulate Data
  # Data Sheet 1
  data <- data |>
    mutate(
      Date = ifelse(is.na(Date), NA,
        format(Sys.Date(), "%d.%m.%Y")
      ),
      Titel = ifelse(is.na(Titel), NA,
        paste("Abstimmungsresultat für Ihre Auswahl:", string_choice)
      )
    )

  selected <- list(c(
    "T_1", "Abstimmungsresultat für Ihre Auswahl:",
    string_choice, " ", " ", "Quelle: Statistik Stadt Zürich, Präsidialdepartement"
  )) |>
    as.data.frame()

  # Data Sheet 2
  # Styling
  sty <- createStyle(fgFill = "#ffffff")
  styConcept <- createStyle(
    textDecoration = c("bold"),
    valign = "top",
    wrapText = TRUE
  )
  styDefinition <- createStyle(
    valign = "top",
    wrapText = TRUE
  )
  styTitle <- createStyle(fontName = "Arial Black")

  # Create Workbook
  wb <- createWorkbook()

  # Add Sheets
  addWorksheet(wb, sheetName = "Inhalt", gridLines = FALSE)
  addWorksheet(wb, sheetName = "T_1", gridLines = TRUE)

  # Write Table Sheet 1
  writeData(wb,
    sheet = 1, x = data,
    colNames = FALSE, rowNames = FALSE,
    startCol = 2,
    startRow = 7,
    withFilter = FALSE
  )

  # Write Table Sheet 2
  writeData(wb,
    sheet = 2, x = selected,
    colNames = FALSE, rowNames = FALSE,
    startCol = 1,
    startRow = 1,
    withFilter = FALSE
  )
  writeData(wb,
    sheet = 2, x = data_for_download,
    colNames = TRUE, rowNames = FALSE,
    startCol = 1,
    startRow = 9,
    withFilter = FALSE
  )

  # Insert Logo on Sheet 1
  insertImage(wb, path_logo, sheet = 1, startRow = 2, startCol = 2, width = 1.75, height = 0.35)

  # Add Styling
  addStyle(wb, 1, style = sty, row = 1:19, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, 1, style = styTitle, row = 14, cols = 2, gridExpand = TRUE)
  addStyle(wb, 2, style = styConcept, row = 9, cols = 1:50, gridExpand = TRUE)
  modifyBaseFont(wb, fontSize = 8, fontName = "Arial")

  # Set Column Width for content
  column_names <- LETTERS[1:14]
  widths <- c(12, 22, 60, 17, 17, 17, 17, 17, 17, 17, 10, 10, 10, 10)
  purrr::map2(
    column_names, widths,
    \(x, y) setColWidths(wb, sheet = 2, cols = x, widths = y)
  )

  # Set Column Width for overview sheet
  setColWidths(wb, sheet = 1, cols = "A", widths = 1)
  setColWidths(wb, sheet = 1, cols = "B", widths = 4)
  setColWidths(wb, sheet = 1, cols = "D", widths = 40)
  setColWidths(wb, sheet = 1, cols = "E", widths = 10)

  # Save Excel
  saveWorkbook(wb, file, overwrite = TRUE) ## save to working directory
}
