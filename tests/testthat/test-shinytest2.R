library(shinytest2)

# for some reason, shinytest2::record_test does not work (possibly a port issue)
# to "see" what the app looks like, insert a `app$get_screenshot("debug.png")` to get a screenshot

test_that("{shinytest2} recording: abda-golem: Test inputs", {
  app <- AppDriver$new(
    name = "abda-golem: Test inputs",
    app_dir = run_app,
    height = 961,
    width = 1416
  )
  # set inputs explicitly as otherwise it defaults to the latest data,
  # which can be updated and therefore change
  app$set_inputs(`input_1-date_range` = c("1933-01-01", "2022-02-14"))
  app$click("action_button")
  app$wait_for_idle()
  app$expect_values(screenshot_args = FALSE)

  app$set_inputs(
    `results_1-show_details` = 4,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$wait_for_idle()
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(`input_1-level_vote` = "Kantonale Vorlagen")
  app$click("action_button")
  app$wait_for_idle()
  app$expect_values(screenshot_args = FALSE)
  app$set_inputs(
    `results_1-show_details` = 3,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  # loading the secondary reactable apparently takes some time
  app$wait_for_idle()
  app$expect_values(screenshot_args = FALSE)
})

test_that("{shinytest2} recording: abda-golem: test download", {
  app <- AppDriver$new(
    name = "abda-golem: test download",
    app_dir = run_app,
    height = 961,
    width = 1416
  )
  app$set_inputs(`input_1-date_range` = c("1933-01-01", "2024-05-06"))
  app$click("action_button")
  app$wait_for_idle() # wait until data is ready
  # check csv
  app$expect_download("download_1-csv_download")

  # check excel for dropdown
  app$set_inputs(
    `results_1-show_details` = 4,
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$wait_for_idle()
  # adjust test for excel: as metadata is different every time, get file and
  # compare only the content
  # not tested like this: the image and the date on the first sheet
  temp_excel_file <- "temp-excel-test.xlsx"
  app$get_download("download_1-excel_download", temp_excel_file)
  sheet1 <- read.xlsx(temp_excel_file, sheet = 1, colNames = F)
  # only test first 3 columns, 4th columns contains date
  expect_snapshot(sheet1[, 1:3])
  sheet2 <- read.xlsx(temp_excel_file, sheet = 2, colNames = F)
  expect_snapshot(sheet2)
  file.remove(temp_excel_file)
})

test_that("{shinytest2} recording: abda-golem: test excel full vote data", {
  app <- AppDriver$new(
    name = "abda-golem: test download",
    app_dir = run_app,
    height = 961,
    width = 1416
  )
  app$set_inputs(`input_1-date_range` = c("1933-01-01", "2024-05-06"))
  app$click("action_button")
  app$wait_for_idle()

  # adjust test for excel: as metadata is different every time, get file and
  # compare only the content
  # not tested like this: the image and the date on the first sheet
  temp_excel_file <- "temp-excel-test.xlsx"
  app$get_download("download_1-excel_download", temp_excel_file)
  sheet1 <- read.xlsx(temp_excel_file, sheet = 1, colNames = F)
  # only test first 3 columns, 4th columns contains date
  expect_snapshot(sheet1[, 1:3])
  sheet2 <- read.xlsx(temp_excel_file, sheet = 2, colNames = F)
  expect_snapshot(sheet2)
  file.remove(temp_excel_file)
})
