#' Calcul des accroissements sur le diamètre
#'
#' @description Pour la première mesure utilise la table AcctD, sinon calcule les accroissements réellement constatés.
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_AcctD <- function(df_Arbres, Cycles) {
  # df_Arbres = arbres

  DernierCycle <- max(df_Arbres$Cycle, na.rm = T)
  PremierCycle <- min(df_Arbres$Cycle, na.rm = T)

  if (DernierCycle == PremierCycle) {
    df_Arbres <- df_Arbres |>
      left_join(AcctD, by = join_by(NumForet, Essence, Cycle, Classe, Strate))
  } else {

    tLaps <- Cycles |>
      dplyr::select(NumForet, Cycle, Annee) |>
      mutate(Inter = Annee - lag(Annee)) |>
      mutate(Inter = ifelse(Cycle == 1, lead(Inter), Inter)) |>
      filter(!is.na(Inter)) |>
      dplyr::select(-Annee)


    df_AcctD <- df_Arbres |>
      arrange(NumForet, Strate, NumPlac, NumArbre, Cycle) |>
      group_by(NumForet, Strate, NumPlac, NumArbre) |>
      mutate(AccD = Diam - lag(Diam)) |>
      mutate(AccD = ifelse(Cycle == 1, lead(AccD), AccD))

    AcctDEssClasse <- df_AcctD |>
      group_by(NumForet,Strate,Cycle,Essence,Classe) |>
      summarise(AccDMoyEssClasse = mean(AccD, na.rm=T),
                Nbre = n()) |>
      filter(Nbre > 1) |>
      dplyr::select(-Nbre)

    AcctDEss <- df_AcctD |>
      group_by(NumForet,Strate,Cycle,Essence) |>
      summarise(AccDMoyEss = mean(AccD, na.rm=T),
                Nbre = n()) |>
      filter(Nbre > 1) |>
      dplyr::select(-Nbre)

    AcctDmoy <- df_AcctD |>
      group_by(NumForet,Strate,Cycle) |>
      summarise(AccDMoy = mean(AccD, na.rm=T),
                Nbre = n()) |>
      filter(Nbre > 1) |>
      dplyr::select(-Nbre)

    AccD <- df_AcctD |>
      dplyr::select(NumForet:NumArbre,Strate, Cycle,Essence,Classe,AccD) |>
      left_join(AcctDEssClasse, by = join_by(NumForet, Strate, Cycle, Essence, Classe)) |>
      left_join(AcctDEss, by = join_by(NumForet, Strate, Cycle, Essence)) |>
      left_join(AcctDmoy, by = join_by(NumForet, Strate, Cycle)) |>
      mutate(AccD = ifelse(is.na(AccD), AccDMoyEssClasse, AccD)) |>
      mutate(AccD = ifelse(is.na(AccD), AccDMoyEss, AccD)) |>
      mutate(AccD = ifelse(is.na(AccD), AccDMoy, AccD)) |>
      dplyr::select(-c(AccDMoyEssClasse,AccDMoyEss,AccDMoy)) |>
      left_join(tLaps, by = join_by(NumForet, Cycle)) |>
      mutate(AccD = AccD / Inter) |>
      dplyr::select(-Inter)


    df_Arbres <- df_Arbres|>
      left_join(AccD, by = join_by(NumForet, NumPlac, NumArbre, Essence, Cycle, Classe, Strate))

  }

  return(df_Arbres)
}
