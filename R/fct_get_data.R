#' get_data
#'
#' @description Function to get the necessary data from the OGD portal and return it in a wrangled form
#'
#'
#' @return a tibble: df_main
#' @export
#'
#' @examples
#' data <- get_data()
get_data <- function() {
  # get parameters for data load (urls)
  url_ogd <- c("https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933/download/abstimmungen_seit1933.csv")


  # download the data
  df <- data.table::fread(url_ogd, encoding = "UTF-8")

  # Prepare Data for analysis
  data <- df |>
    wrangle_data()

  # adjust names to something suitable, both for variables as well as functions
  return("df_main" = data)
}
