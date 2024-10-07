#' Calcul des variables dendrométriques, économiques et écologiques.
#'
#' @description Cette fonction commence par calculer le poids de chaque arbre,
#' puis pour chacun d'eux, calcule les variables dendrométriques, économiques et écologiques ramenées à l'hectare.
#'
#' @return La fonction construit les tables suivantes.
#'
#' arbres, Reges, Taillis, Reperes, BMSLineaires, BMSsup30, BMP, Codes
#' Elles sont enregistrées dans le dossier Tables sous le nom : gfTablesBrutes.RData.
#'
#' @param TauxR = taux d'actualisation (0.03 par défaut)
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


gf_Calculs <- function(TauxR=0.03) {

  #--------------- Preambule ---------------
  if (!("gfDonneesBrutes.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Xls2Rdata")
  } else {load("Tables/gfDonneesBrutes.RData")}

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
  # noms = c("NumForet","NumPlac","Cycle","Strate","PoidsPlacette","Pente","CoeffPente")
  arbres <- arbres |>
    left_join(Placettes[,1:7], by=c("NumForet","NumPlac","Cycle"))

  #--------------- Calculs poids ---------------
  arbres <- Calculs_Poids(arbres,Echantillonnages)

  #--------------- Calculs Gha ---------------
  arbres <- Calculs_Gha(arbres)

  #--------------- Calculs volumes ---------------
  arbres <- Calculs_Volumes(arbres,Tarifs)


  #--------------- Sauvegarde ---------------
  writexl::write_xlsx(arbres, "arbres.xlsx")



  return(arbres)
}
