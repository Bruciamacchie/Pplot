#' Aggreg placettes.
#'
#' @description Aggrégation des donnéese par placettes.
#'
#' @return La fonction construit la table
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


gf_AggregPlac <- function() {

  #--------------- Preambule ---------------
  if (!("gftablesElaborees.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Xls2Rdata")
  } else {load("Tables/gftablesElaborees.Rdata")}



  return(arbres)
}
