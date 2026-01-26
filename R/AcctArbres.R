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
  # df_Arbres = Arbres

  load("Tables/Param.Rdata")

  Acct_arbres <- df_Arbres |>
    arrange(NumForet, Strate, NumPlac, NumArbre, Cycle) |>
    dplyr::select(NumForet, Strate, NumPlac, NumArbre,Essence,Classe, Cycle,Statut,
                  Diam,Nha,Gha,Vha,VcHa,VpHa, GainHa) |>
    group_by(NumForet, Strate, NumPlac, NumArbre,Statut) |>
    mutate(across(c(Gha, Vha, VcHa, VpHa, GainHa),
                  ~ if_else(Statut == "Reste", .x - lag(.x),
                            ifelse(Statut == "Nouveau", .x, 0)),
                  .names = "Acct{.col}Per")) |>
    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>
    filter(Cycle > 1) |>
    rename(AccGper = AcctGhaPer) |>
    rename(AccVper = AcctVhaPer) |>
    rename(AccVcper = AcctVcHaPer) |>
    rename(AccVpper = AcctVpHaPer) |>
    rename(AccGainper = AcctGainHaPer)

  MoyAcctEssClasse <- Acct_arbres |>
    group_by(NumForet, Strate, NumPlac, Essence,Classe) |>
    summarise(AccGperEC = mean(AccGper, na.rm=T),
              AccVperEC = mean(AccVper, na.rm=T),
              AccVcperEC = mean(AccVcper, na.rm=T),
              AccVpperEC = mean(AccVpper, na.rm=T),
              AccGainperEC = mean(AccGainper, na.rm=T))

  MoyAcctEss <- Acct_arbres |>
    group_by(NumForet, Strate, NumPlac, Essence) |>
    summarise(AccGperE = mean(AccGper, na.rm=T),
              AccVperE = mean(AccVper, na.rm=T),
              AccVcperE = mean(AccVcper, na.rm=T),
              AccVpperE = mean(AccVpper, na.rm=T),
              AccGainperE = mean(AccGainper, na.rm=T))

  Complements <- MoyAcctEssClasse |>
    left_join(MoyAcctEss, by = join_by(NumForet, Strate, NumPlac, Essence)) |>
    mutate(AccGperEC = ifelse(is.na(AccGperEC), AccGperE , AccGperEC)) |>
    mutate(AccVperEC = ifelse(is.na(AccVperEC), AccVperE , AccVperEC)) |>
    mutate(AccVcperEC = ifelse(is.na(AccVcperEC), AccVcperE , AccVcperEC)) |>
    mutate(AccVpperEC = ifelse(is.na(AccVpperEC), AccVpperE , AccVpperEC)) |>
    mutate(AccGainperEC = ifelse(is.na(AccGainperEC), AccGainperE , AccGainperEC))

  Acct_arbres <- Acct_arbres |>
    left_join(Complements, by = join_by(NumForet, Strate, NumPlac, Essence, Classe)) |>
    mutate(AccGper = ifelse(Statut == "Coupe",AccGperEC/2, AccGper)) |>
    mutate(AccVper = ifelse(Statut == "Coupe",AccVperEC/2, AccVper)) |>
    mutate(AccVcper = ifelse(Statut == "Coupe",AccVcperEC/2, AccVcper)) |>
    mutate(AccVpper = ifelse(Statut == "Coupe",AccVpperEC/2, AccVpper)) |>
    mutate(AccGainper = ifelse(Statut == "Coupe",AccGainperEC/2, AccGainper)) |>
    dplyr::select(-c(AccGperEC:AccGainperE))

  return(Acct_arbres)
}
