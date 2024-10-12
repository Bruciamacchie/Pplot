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

  #--------------- Acct placettes ---------------
  AcctPlacettes <- AcctPlacettes(df_Arbres)


  return(AcctPlacettes)
}
