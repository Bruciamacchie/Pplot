#' Evolution arbres
#'
#' @description Evolution des principaux indicateurs par arbre.
#'
#' @return La fonction analyse pour chaque arbrel'évolution
#' des principaux indicateurs, Gha, Vha, VcHa, VpHa, GainHa.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


AcctArbres <- function(df_Arbres) {
  # df_Arbres = Arbres1

  DernierCycle <- max(df_Arbres$Cycle, na.rm = T)
  PremierCycle <- min(df_Arbres$Cycle, na.rm = T)
  période = 9

  # # -------- Global
  # Acct_placettes <- df_Arbres |>
  #   arrange(NumForet, Strate, NumPlac, NumArbre, Cycle) |>
  #   group_by(Cycle, Statut,NumPlac) |>
  #   summarise(Gha = sum(Gha),
  #             Vha = sum(Vha),
  #             VcHa = sum(VcHa),
  #             VpHa = sum(VpHa),
  #             GainHa = sum(GainHa)) |>
  #   pivot_longer(cols = Gha:GainHa) |>
  #   group_by(name) |>
  #   pivot_wider(names_from = Statut, values_from = value, values_fill=0) |>
  #   mutate(Acct = (Reste - lag(Reste) + Nouveau + lag(Coupe))/période)
  #
  # Coupes <- t1 |>
  #   filter(Cycle < DernierCycle) |>
  #   dplyr::select(Cycle, NumPlac, name, Coupe)
  #
  # Acctper <- t1 |>
  #   filter(Cycle > PremierCycle) |>
  #   dplyr::select(Cycle, NumPlac, name, Acct)

  Acct_arbres <- df_Arbres |>
    arrange(NumForet, Strate, NumPlac, NumArbre, Cycle) |>
    dplyr::select(NumForet, Strate, NumPlac, NumArbre, Cycle,Statut,
                  Diam,Nha,Gha,Vha,VcHa,VpHa, GainHa) |>
    group_by(NumForet, Strate, NumPlac, NumArbre) |>
    mutate(AccGper = ifelse(Statut == "Reste", Gha - lag(Gha), 0)) |>
    mutate(AccGper = ifelse(Statut == "Nouveau", Gha, AccGper)) |>
    mutate(AccGper = ifelse(Statut == "Coupe", Gha, AccGper)) |>
    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>

    mutate(AccVper = ifelse(Statut == "Reste", Vha - lag(Vha), 0)) |>
    mutate(AccVper = ifelse(Statut == "Nouveau", Vha, AccVper)) |>
    mutate(AccVper = ifelse(Statut == "Coupe", Vha, AccVper)) |>
    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>

    mutate(AccVcper = ifelse(Statut == "Reste", VcHa - lag(VcHa), 0)) |>
    mutate(AccVcper = ifelse(Statut == "Nouveau", VcHa, AccVcper)) |>
    mutate(AccVcper = ifelse(Statut == "Coupe", VcHa, AccVcper)) |>
    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>

    mutate(AccVpper = ifelse(Statut == "Reste", VpHa - lag(VpHa), 0)) |>
    mutate(AccVpper = ifelse(Statut == "Nouveau", VpHa, AccVpper)) |>
    mutate(AccVpper = ifelse(Statut == "Coupe", VpHa, AccVpper)) |>
    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>

    mutate(AccGainper = ifelse(Statut == "Reste", GainHa - lag(GainHa), 0)) |>
    mutate(AccGainper = ifelse(Statut == "Nouveau", GainHa, AccGainper)) |>
    mutate(AccGainper = ifelse(Statut == "Coupe", GainHa, AccGainper)) |>
    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>

    filter(Cycle > 1)

  return(Acct_arbres)
}
