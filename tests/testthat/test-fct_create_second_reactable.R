test_that("test if second reactable is created, check filter function", {
  my_inputs <- list(
    "search_text" = "",
    "level_vote" = "Alle Vorlagen",
    "date_range" = range(df_main$Datum)
  )
  filtered_data <- filter_data_vote(filter_data_with_inputs(df_main, my_inputs), 1)
  name_vote <- name_date_vote(filter_data_with_inputs(df_main, my_inputs), 1)
  
  # check data type
  expect_s3_class(
    get_second_reactable(filtered_data, name_vote),
    "reactable"
  )
  # check dimensions
  expect_equal(
    length(get_second_reactable(filtered_data, name_vote)$x$tag$attribs$columns),
    6 # however many columns are selected within the function
  )
})
