test_that("test filter data for reactable, check if reactable is created", {
  # prepare inputs
  my_inputs <- list(
    "search_text" = "",
    "level_vote" = "Alle Vorlagen",
    "date_range" = range(df_main$Datum)
  )
  filtered_data <- filter_data_with_inputs(df_main, my_inputs)

  # check data type
  expect_s3_class(
    create_main_reactable(filtered_data, 3),
    "reactable"
  )
  # check dimensions
  expect_equal(
    length(create_main_reactable(filtered_data, 3)$x$tag$attribs$columns),
    3 # however many columns are selected within the function
  )
})
