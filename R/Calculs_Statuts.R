#' Calcul statuts
#'
#' @description Décomposition des tiges entre celles qui restent, celles coupées et les nouvelles.
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_Statuts <- function(df_Arbres) {
  # df_Arbres = arbres

  DernierCycle <- max(df_Arbres$Cycle, na.rm = T)
  PremierCycle <- min(df_Arbres$Cycle, na.rm = T)

  df_Arbres1 <- df_Arbres |>
    arrange(NumForet, Strate, NumPlac, NumArbre, Cycle, Diam) |>
    dplyr::select(NumForet, Strate, NumPlac, Cycle, NumArbre,Diam,Dist) |>
    group_by(NumForet, Strate, NumPlac, NumArbre) |>
    mutate(Avant = lag(Diam),
           Après = lead(Diam)) |>
    mutate(Statut = case_when(
      Cycle == PremierCycle & is.na(Avant) & !is.na(Après) ~ "Reste",
      Cycle == DernierCycle & !is.na(Avant) & is.na(Après) ~ "Reste",
      !is.na(Avant) & !is.na(Après) ~ "Reste",
      is.na(Avant) & Cycle > PremierCycle & !is.na(Après) ~ "Nouveau",
      is.na(Avant) & is.na(Après) & Cycle == DernierCycle  ~ "Nouveau",
      is.na(Après) & Cycle < DernierCycle ~ "Coupe")) |>
    mutate(Statut = ifelse(Diam < Echantillonnages$DiamLim1[2], "Protocole",Statut)) |>
    dplyr::select(-c(Avant, Après))

  df_Arbres <- df_Arbres |>
    left_join(df_Arbres1, by = join_by(NumForet, NumPlac, NumArbre, Cycle, Diam, Strate))

  Coupes <- df_Arbres |>
    filter(Statut == "Coupe") |>
    dplyr::select(NumForet:Essence,Cycle,Gha, Vha:Statut)

  save(Coupes, file = "Tables/Coupes.Rdata")

  return(df_Arbres)
}


