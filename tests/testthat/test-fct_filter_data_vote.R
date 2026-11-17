test_that("check if data is filteres with inputs, check if individual vote is filtered", {
  my_inputs <- list(
    "search_text" = "",
    "level_vote" = "Eidgenössische Vorlagen",
    "date_range" = range(df_main$Datum)
  )
  filtered_data <- filter_data_vote(filter_data_with_inputs(df_main, my_inputs), 1)
  
  expect_equal(
    ncol(filtered_data),
    11
  )
  
  name_vote <- name_date_vote(filter_data_with_inputs(df_main, my_inputs), 1)
  
  expect_equal(
    ncol(name_vote),
    3
  )
  
  expect_equal(
    nrow(name_vote),
    1
  )
  
  # check if filter function works
  initial_filter <- df_main |>
    filter(`Politische Ebene` == my_inputs$level_vote)
  expect_equal(
    filter_data_with_inputs(df_main, my_inputs),
    initial_filter
  )
  
  
})
