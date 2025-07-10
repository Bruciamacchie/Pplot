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
    group_by(NumForet, Strate, NumPlac, NumArbre) |>
    mutate(AccGper = ifelse(Statut == "Reste", Gha - lag(Gha), 0)) |>
    mutate(AccGper = ifelse(Statut == "Nouveau", Gha, AccGper)) |>
    mutate(AccGper = ifelse(Statut == "Coupe", Gha, AccGper)) |>

    mutate(AccVper = ifelse(Statut == "Reste", Vha - lag(Vha), 0)) |>
    mutate(AccVper = ifelse(Statut == "Nouveau", Vha, AccVper)) |>
    mutate(AccVper = ifelse(Statut == "Coupe", Vha, AccVper)) |>

    mutate(AccVcper = ifelse(Statut == "Reste", VcHa - lag(VcHa), 0)) |>
    mutate(AccVcper = ifelse(Statut == "Nouveau", VcHa, AccVcper)) |>
    mutate(AccVcper = ifelse(Statut == "Coupe", VcHa, AccVcper)) |>

    mutate(AccVpper = ifelse(Statut == "Reste", VpHa - lag(VpHa), 0)) |>
    mutate(AccVpper = ifelse(Statut == "Nouveau", VpHa, AccVpper)) |>
    mutate(AccVpper = ifelse(Statut == "Coupe", VpHa, AccVpper)) |>

    mutate(AccGainper = ifelse(Statut == "Reste", GainHa - lag(GainHa), 0)) |>
    mutate(AccGainper = ifelse(Statut == "Nouveau", GainHa, AccGainper)) |>
    mutate(AccGainper = ifelse(Statut == "Coupe", GainHa, AccGainper)) |>

    mutate(Cycle = ifelse(Statut == "Coupe", Cycle+1, Cycle)) |>

    filter(Cycle > 1)

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

  Acct_arbres <- Acct_arbres |>
    left_join(MoyAcctEssClasse, by = join_by(NumForet, Strate, NumPlac, Essence, Classe)) |>
    # left_join(MoyAcctEss, by = join_by(NumForet, Strate, NumPlac, Essence)) |>
    mutate(AccVcper = ifelse(Statut == "Coupe",AccVcper + AccVcperEC/2, AccVcper))

  return(Acct_arbres)
}
