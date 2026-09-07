#' wrangle_data
#'
#' function to wrangle the data (applicable to all data)
#'
#' @param df data.frame with data
#'
#' @return wrangled data.frame
#' @noRd
wrangle_data <- function(df) {
  df |>
    dplyr::mutate(Abstimmungs_Datum = as.Date(Abstimmungs_Datum)) |>
    dplyr::mutate(Name_Resultat_Gebiet = case_when(
      Name_Resultat_Gebiet == "Stadt Zürich" & !is.na(Nr_Wahlkreis_StZH) ~ "Stadtkreise",
      TRUE ~ Name_Resultat_Gebiet
    )) |>
    dplyr::mutate(Name_Politische_Ebene = case_when(
      Name_Politische_Ebene == "Eidgenossenschaft" ~ "Eidgenössische Vorlagen",
      Name_Politische_Ebene == "Stadt Zürich" ~ "Städtische Vorlagen",
      Name_Politische_Ebene == "Kanton Zürich" ~ "Kantonale Vorlagen",
    )) |>
    dplyr::mutate(Name_Resultat_Gebiet = case_when(
      Name_Resultat_Gebiet == "Stadtkreise" ~ Name_Wahlkreis_StZH,
      Name_Resultat_Gebiet == "Eidgenossenschaft" ~ "Gesamte Schweiz",
      TRUE ~ Name_Resultat_Gebiet
    )) |>
    dplyr::mutate(NrGebiet = case_when(
      !is.na(Nr_Wahlkreis_StZH) ~ as.numeric(Nr_Wahlkreis_StZH * 10),
      TRUE ~ as.numeric(Nr_Resultat_Gebiet)
    )) |>
    dplyr::mutate(Stimmberechtigt = case_when(
      Stimmberechtigt == 0 ~ NA_integer_,
      TRUE ~ Stimmberechtigt
    )) |>
    # Auslandschweizer/-innen streichen
    filter(Name_Resultat_Gebiet != "Auslandschweizer/-innen") |>
    # Rename variables
    dplyr::rename(
      Abstimmungstext = Abstimmungs_Text,
      Datum = Abstimmungs_Datum,
      "Politische Ebene" = Name_Politische_Ebene,
      "Wahlkreis" = Name_Wahlkreis_StZH,
      "Gebiet" = Name_Resultat_Gebiet,
      "Stimmberechtigte" = Stimmberechtigt,
      "Ja-Stimmen" = Ja_Absolut,
      "Nein-Stimmen" = Nein_Absolut,
      "Stimmbeteiligung (in %)" = Stimmbeteiligung_Prozent,
      "Ja-Anteil (in %)" = Ja_Prozent,
      "Nein-Anteil (in %)" = Nein_Prozent,
      "Stände Ja" = Staende_Ja,
      "Stände Nein" = Staende_Nein
    )

}
