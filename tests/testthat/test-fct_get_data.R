test_that("check if OGD download of data works, check if variables, datatypes and NAs are as excpected", {
  df <- get_data()
  expect_s3_class(df, "data.table")
  
  expect_named(df, c("Datum", "Nr_Politische_Ebene", "Politische Ebene", "Abstimmungstext", "Nr_Resultat_Gebiet", "Gebiet", "Nr_Wahlkreis_StZH", "Wahlkreis", "Stimmberechtigte", "Ja-Stimmen", "Nein-Stimmen", "Stimmbeteiligung (in %)", "Ja-Anteil (in %)", "Nein-Anteil (in %)", "Stände Ja", "Stände Nein", "NrGebiet"))
  
  # Haben Variablen den erwarteten Datentyp?
  col_names_df <- c("Datum", "Nr_Politische_Ebene", "Politische Ebene", "Abstimmungstext", "Nr_Resultat_Gebiet", "Gebiet", "Stimmberechtigte", "Ja-Stimmen", "Nein-Stimmen", "Stimmbeteiligung (in %)", "Ja-Anteil (in %)", "Nein-Anteil (in %)")
  col_types <- c("double", "integer", "character",  "character", "integer", "character", "integer", "integer", "integer", "double", "double", "double")
  purrr::map2(col_names_df, col_types,
              \(x, y) expect_type(df[[x]], y))

  # Welche Spalten dürfen keine Missings haben?
  col_names_nona <- c("Datum", "Nr_Politische_Ebene", "Politische Ebene", "Abstimmungstext", "Nr_Resultat_Gebiet", "Gebiet", "Ja-Stimmen", "Nein-Stimmen", "Stimmbeteiligung (in %)", "Ja-Anteil (in %)", "Nein-Anteil (in %)")
  purrr::map(col_names_nona, 
              \(x) expect_false(any(is.na(df[[x]])))
             )
  
  # Sind mindestens 2600 Abstimmungen im df 
  expect_gt(length(unique(df$Abstimmungstext)), 2500)
  
})


