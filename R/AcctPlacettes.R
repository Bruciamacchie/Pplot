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
  # df_Arbres = Arbres1

  DernierCycle <- max(df_Arbres$Cycle, na.rm = T)
  PremierCycle <- min(df_Arbres$Cycle, na.rm = T)
  période = 9

  t1 <- df_Arbres |>
    arrange(NumForet, Strate, NumPlac, NumArbre, Cycle) |>
    group_by(Cycle, Statut,NumPlac) |>
    summarise(Gha = sum(Gha),
              Vha = sum(Vha),
              VcHa = sum(VcHa),
              VpHa = sum(VpHa),
              GainHa = sum(GainHa)) |>
    pivot_longer(cols = Gha:GainHa) |>
    group_by(name) |>
    pivot_wider(names_from = Statut, values_from = value, values_fill=0) |>
    mutate(Acct = (Reste - lag(Reste) + Nouveau + lag(Coupe))/période)

  Coupes <- t1 |>
    filter(Cycle < DernierCycle) |>
    dplyr::select(Cycle, NumPlac, name, Coupe) |>
    mutate(Cycle = Cycle + 1)

  Acct_placettes <- t1 |>
    filter(Cycle > PremierCycle) |>
    dplyr::select(Cycle, NumPlac, name, Acct) |>
    left_join(Coupes, by = join_by(Cycle, NumPlac, name))

  return(Acct_placettes)
}
