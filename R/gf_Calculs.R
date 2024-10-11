#' Calcul des variables dendrométriques, économiques et écologiques.
#'
#' @description Cette fonction commence par calculer le poids de chaque arbre,
#' puis pour chacun d'eux, calcule les variables dendrométriques, économiques et écologiques ramenées à l'hectare.
#'
#' @return La fonction construit la table Arbres.
#'
#' @param TauxR = taux d'actualisation
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


gf_Calculs <- function(tauxR) {

  #--------------- Preambule ---------------
  if (!("gfDonneesBrutes.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Xls2Rdata")
  } else {load("Tables/gfDonneesBrutes.RData")}

  #--------------- Table arbres ---------------
  arbres <- IdArbres |>
    right_join(ValArbres, by="IdArbre") |>
    filter(!is.na(NumArbre)) |>
    mutate(Diam = (Diam1 + Diam2)/2,
           Classe = floor(Diam/5+0.5)*5,
           Cat = cut(Diam, breaks = c(0, 17.5, 27.5, 47.5, 67.5,200),
                     labels = c("PER", "PB", "BM", "GB","TGB"), include.lowest = T, right = F)) |>
    dplyr::select(-IdArbre) |>
    filter(Diam>=7.5)

  #--------------- Fusion Placettes ---------------
  Pla <- Placettes |>
    dplyr::select(NumForet:Miroir_Dist)

  arbres <- arbres |>
    left_join(Pla, by=c("NumForet","NumPlac","Cycle"))

  #--------------- Traitement placettes miroirs ---------------
  arbres <- Calculs_Miroirs(arbres,Echantillonnages)


  #--------------- Calculs poids ---------------
  arbres <- Calculs_Poids(arbres,Echantillonnages)

  #--------------- Calculs Gha ---------------
  arbres <- Calculs_Gha(arbres)

  #--------------- Calculs volumes ---------------
  arbres <- Calculs_Volumes(arbres,Tarifs)

  #--------------- Calculs AcctD ---------------
  arbres <- Calculs_AcctD(arbres, Cycles)

  #--------------- Calculs valeurs ---------------
  arbres <- Calculs_Valeurs(arbres, Quals, Prix,tauxR)

  #--------------- Calculs valuesHA ---------------
  arbres <- Calculs_Ha(arbres)

  #--------------- Note écologique ---------------

  #--------------- Carbone ---------------




  #--------------- Sauvegarde ---------------
  writexl::write_xlsx(arbres, "arbres.xlsx")

  dir.create("Tables", showWarnings = F)
  save(arbres,Placettes,
       file = "Tables/gfTablesElaborees.Rdata")

  return(arbres)
}
