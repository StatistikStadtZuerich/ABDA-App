test_that("test if data for download is selected correctly", {
  
  my_inputs <- list(
    "search_text" = "",
    "level_vote" = "Alle Vorlagen",
    "date_range" = range(df_main$Datum)
  )
  filtered_data <- filter_data_with_inputs(df_main, my_inputs)
  
  expect_equal(
    ncol(select_data_download(filtered_data)),
    10
  )
  
})
