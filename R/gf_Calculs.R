#' Calcul des variables dendrométriques, économiques et écologiques.
#'
#' @description Cette fonction commence par calculer le poids de chaque arbre,
#' puis pour chacun d'eux, calcule les variables dendrométriques, économiques et écologiques ramenées à l'hectare.
#'
#' @return La fonction construit la table Arbres.
#'
#' @param tauxR = taux d'actualisation
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


gf_Calculs <- function(fich, tauxR) {
  setwd(dirname(fich))


  #--------------- Preambule ---------------
  if (!("gfDonneesBrutes.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Xls2Rdata")
  } else {load("Tables/gfDonneesBrutes.RData")}

  #--------------- Différentes tables ---------------
  Arbres  <- gf_Calculs_arbres(fich, tauxR)
  BMSl    <- Calculs_BMS_lineaire(BMSLineaires, EssReg)
  BMSc    <- Calculs_BMS_lineaire(BMSCercles, EssReg)
  BMS = rbind(BMSl, BMSc)
  RegesHa   <- Calculs_Reges(Reges)

  temp <- PCQM |>  filter(Population == "BMP")
  BMP  <- Calcul_BMP_pcqm(temp)

  PerchesCercle <- Arbres |>
    filter(Cat == "PER") |>
    mutate(FH = Vha/Gha)
  FHmoy = mean(PerchesCercle$FH, na.rm=T)
  PerchesCercle <- PerchesCercle |>
    dplyr::select(NumForet,NumPlac,Cycle,Essence,Azimut,Dist,Diam,Haut,Nha,Gha,Vha)

  temp <- PCQM |>  filter(Population == "Perche")
  PerchesPCQM  <- Calcul_perches_pcqm(temp) |>
    dplyr::select(names(PerchesCercle)) |>
    mutate(Vha = Gha * FHmoy)

  Perches <- rbind(PerchesCercle,PerchesPCQM)

  NoteEcolos <- dmh_split(Arbres)




  #--------------- Sauvegarde ---------------
  # writexl::write_xlsx(arbres, "arbres.xlsx")

  dir.create("Tables", showWarnings = F)
  save(BMS, BMP, Perches, file = "Tables/gfTablesPCQM.Rdata")
  save(RegesHa, file = "Tables/RegesHa.Rdata")
  save(Arbres, file = "Tables/Arbres.Rdata")

  return(arbres)
}
