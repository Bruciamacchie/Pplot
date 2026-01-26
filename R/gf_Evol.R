#' Evolution des peuplemernts.
#'
#' @description Caractérisation de l'évolution des peuplements dès la première
#' remesure.
#'
#' @return La fonction construit la table Arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'


gf_Evol <- function (df_Arbres) {
  # df_Arbres = Arbres

  #--------------- Calculs Statuts ---------------
  df_Arbres <- Calculs_Statuts(df_Arbres)

  #--------------- Acct arbres ---------------
  DeltaArbres <- AcctArbres(df_Arbres)
  save(DeltaArbres, file="Tables/DeltaArbres.Rdata")

  #--------------- Acct placettes ---------------
  EvolPlacettes <- AcctPlacettes(DeltaArbres)
  save(EvolPlacettes, file="Tables/EvolPlacettes.Rdata")

  return(EvolPlacettes)
}
