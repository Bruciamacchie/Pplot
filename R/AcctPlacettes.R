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
  # df_Arbres = DeltaArbres
  load("Tables/Param.Rdata")

  # DernierCycle <- max(df_Arbres$Cycle, na.rm = T)
  # PremierCycle <- min(df_Arbres$Cycle, na.rm = T)

  # période = 9

  Acct_placettes <- df_Arbres |>
    group_by(NumForet, Strate, Cycle, NumPlac) |>
    summarise(across(c(AccGper:AccGainper), sum)) |>
    pivot_longer(cols = AccGper:AccGainper) |>
    left_join(périodes, by = join_by(NumForet, Cycle)) |>
    mutate(Acct = value/période)

  Coupes <- df_Arbres |>
    filter(Statut == "Coupe") |>
    group_by(NumForet, Strate, Cycle, NumPlac) |>
    summarise(across(c(Gha:GainHa), sum)) |>
    pivot_longer(cols = Gha:GainHa) |>
    left_join(périodes, by = join_by(NumForet, Cycle)) |>
    mutate(Coupe = value/période) |>
    dplyr::select(NumForet:name,Coupe)

  # Coupes <- Acct_placettes |>
  #   dplyr::select(NumForet:Coupe) |>
  #   filter(Cycle < DernierCycle) |>
  #   mutate(Cycle = Cycle + 1) |>
  #   left_join(périodes, by = join_by(NumForet, Cycle)) |>
  #   mutate(Coupe = Coupe / période) |>
  #   dplyr::select(NumForet, Strate, Cycle, NumPlac, name, Coupe)

  save(Coupes, file="Tables/Coupes.Rdata")

  return(Acct_placettes)
}
