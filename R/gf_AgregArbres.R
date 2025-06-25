#' Aggrégation des arbres à l'échelle de la placette.
#'
#' @description Sous-programme permettant d'aggréger toutes les variables à l'échelle de la placette.
#' Il fournit de nombreux tableaux croisés.
#' @return Les différentes tables sont enregistrées dans le dossier Tables
#'
#' @author Bruciamacchie Max
#' @import tidyverse
#'
#' @export

gf_AgregArbres <- function(rep) {
  if (!("gfDonneesBrutes.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Xls2Rdata")
  } else {load("Tables/gfDonneesBrutes.RData")}

  if (!("gfTablesElaborees.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Calculs")
  } else {load("Tables/gfTablesElaborees.Rdata")}

  ############################## Tableaux arbres #####################
  ListePlacettes <- Placettes |>
    distinct(NumForet,Cycle,Strate,NumPlac) # Cycle permet d'avoir un nombre de placettes qui évolue

  NbrePlac <- ListePlacettes |>
    group_by(NumForet,Cycle,Strate) |>
    summarise(Nbre = n())

  gfPla <- DeltaArbres |>
    group_by(NumForet, Strate, Cycle, NumPlac) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaDendro <- ListePlacettes |>
    left_join(gfPlaDendro, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Dendro -------------
  gfPlaDendro <- arbres |>
    group_by(NumForet, Strate, Cycle, NumPlac) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaDendro <- ListePlacettes |>
    left_join(gfPlaDendro, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Dendro * Cat -------------
  gfPlaCat <- arbres |>
    group_by(NumForet, Strate, Cycle, NumPlac,Cat) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaCat <- ListePlacettes |>
    left_join(gfPlaCat, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Dendro * Essence * Cat -------------
  gfPlaEssCat <- arbres |>
    left_join(EssReg, by = c("NumForet","Essence")) |>
    group_by(NumForet, Strate, Cycle, NumPlac,EssReg,Cat) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaEssCat <- ListePlacettes |>
    left_join(gfPlaEssCat, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce, 0)

  # ------------- Dendro * Essence -------------
  gfPlaEss <- arbres |>
    left_join(EssReg, by = c("NumForet","Essence")) |>
    group_by(NumForet, Strate, Cycle, NumPlac,EssReg) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaEss <- ListePlacettes |>
    left_join(gfPlaEss, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Dendro * Essence * Classe -------------
  gfPlaEssClasse <- arbres |>
    left_join(EssReg, by = c("NumForet","Essence")) |>
    group_by(NumForet, Strate, Cycle, NumPlac,EssReg,Classe) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaEssClasse <- ListePlacettes |>
    left_join(gfPlaEssClasse, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Dendro * Essence * Classe * Qual -------------
  gfPlaEssClasseQual <- arbres |>
    left_join(EssReg, by = c("NumForet","Essence")) |>
    group_by(NumForet, Strate, Cycle, NumPlac,EssReg,Classe,Qual) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,GainHa), sum))

  gfPlaEssClasseQual <- ListePlacettes |>
    left_join(gfPlaEssClasseQual, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Dendro * Classe -------------
  gfPlaClasse <- arbres |>
    group_by(NumForet, Strate, Cycle, NumPlac,Classe) |>
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa), sum))

  gfPlaClasse <- ListePlacettes |>
    left_join(gfPlaClasse, by = join_by(NumForet, Cycle, Strate, NumPlac)) |>
    mutate_if(is.numeric,coalesce,0)

  # ------------- Sauvegarde -------------
  save(gfPlaCat,gfPlaClasse,gfPlaDendro,gfPlaEss,gfPlaEssCat,gfPlaEssClasse,gfPlaEssClasseQual,
       file="Tables/gfTablesElaboreesPlac.RData")
}





