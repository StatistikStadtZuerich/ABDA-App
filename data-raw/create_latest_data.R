# script to get the latest data from ogd and save it locally
# run locally, and will be run also in the deployment pipeline
#
# when running locally: load all as well
pkgload::load_all(attach_testthat = FALSE)

# use the functions in the R subfolders to get the data from the OGD server
# and prepare them as needed
df_main <- get_data()

# prepare choices (if compuataion-intensive, add this to the package data)
choices_level <- df_main |>
  select(Nr_Politische_Ebene, `Politische Ebene`) |>
  distinct() |>
  arrange(desc(Nr_Politische_Ebene)) |> 
  pull(`Politische Ebene`)

usethis::use_data(df_main, choices_level,
  overwrite = TRUE,
  internal = TRUE
)
