#' Evolution Placettes
#'
#' @description Evolution des principaux indicateurs par placette
#'
#' @return La fonction analyse pour chaque placette l'évolution
#' des principaux indicateurs, Gha, Vha, VcHa, VpHa, GainHa.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


AcctPlacettes <- function(df_Arbres) {
  # df_Arbres = Arbres
  load("Tables/Param.Rdata")

  # DernierCycle <- max(df_Arbres$Cycle, na.rm = T)
  # PremierCycle <- min(df_Arbres$Cycle, na.rm = T)

  # période = 9

  t1 <- df_Arbres |>
    arrange(NumForet, Strate, NumPlac, NumArbre, Cycle) |>
    group_by(NumForet, Strate, Cycle, Statut, NumPlac) |>
    summarise(Gha = sum(Gha),
              Vha = sum(Vha),
              VcHa = sum(VcHa),
              VpHa = sum(VpHa),
              GainHa = sum(GainHa)) |>
    pivot_longer(cols = Gha:GainHa) |>
    group_by(NumForet,Strate,name,Cycle) |>
    pivot_wider(names_from = Statut, values_from = value, values_fill=0) |>
    arrange(NumForet, Strate, NumPlac, name, Cycle) |>
    left_join(périodes, by = join_by(NumForet, Cycle)) |>
    group_by(NumForet,Strate,NumPlac,name) |>
    mutate(Acct = (Reste - lag(Reste) + Nouveau + lag(Coupe))/période)

  Coupes <- t1 |>
    dplyr::select(NumForet:Coupe) |>
    filter(Cycle < DernierCycle) |>
    mutate(Cycle = Cycle + 1) |>
    left_join(périodes, by = join_by(NumForet, Cycle)) |>
    mutate(Coupe = Coupe / période) |>
    dplyr::select(NumForet, Strate, Cycle, NumPlac, name, Coupe)

  save(Coupes, file="Tables/Coupes.Rdata")

  Acct_placettes <- t1 |>
    filter(Cycle > PremierCycle) |>
    dplyr::select(NumForet, Strate, Cycle, NumPlac, name, Acct) |>
    left_join(Coupes, by = join_by(NumForet, Strate, Cycle, NumPlac, name))

  return(Acct_placettes)
}
