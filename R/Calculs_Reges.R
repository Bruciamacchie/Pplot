#' Calculs régénération
#'
#' @description Importance de la régénération par essence et classe de hauteur.
#'
#' @return La fonction prépare l'analyse de la régénération.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

# df = Reges
Calculs_Reges <- function(df) {
  if (dim(df)[1] > 0) {
    load("Tables/gfDonneesBrutes.RData")

    t1 <- EssReg |>
      dplyr::select(NumForet:EssReg)
    t2 <- Echantillonnages |>
      dplyr::select(NumForet:Strate,NbSousPlac,RayonSousPlac)

    df <- df |>
      left_join(t1, by = join_by(NumForet, Essence)) |>
      left_join(t2, by = join_by(NumForet, Cycle)) |>
      filter(!is.na(Essence)) |>
      arrange(NumForet, Cycle, NumPlac, SsPlac) |>
      mutate(

        Recouv = ifelse(is.na(Recouv), 0, Recouv),
        Class1 = ifelse(is.na(Class1), 0, Class1),
        Class2 = ifelse(is.na(Class2), 0, Class2),
        Class3 = ifelse(is.na(Class3), 0, Class3),

        Recouv = as.numeric(Recouv),
        Class1 = as.numeric(Class1),
        Class2 = as.numeric(Class2),
        Class3 = as.numeric(Class3),

        Recouv = Recouv / NbSousPlac,
        Classe1Ha = Class1 * 10000 / (pi * RayonSousPlac ^ 2) / NbSousPlac,
        Classe2Ha = Class2 * 10000 / (pi * RayonSousPlac ^ 2) / NbSousPlac,
        Classe3Ha = Class3 * 10000 / (pi * RayonSousPlac ^ 2) / NbSousPlac
      ) |>
    dplyr::select(NumForet:Cycle,Strate,Essence,EssReg,Abroutis,Recouv,Classe1Ha:Classe3Ha)
  } else {
    df[,c("Classe1Ha","Classe1Ha","Classe1Ha")] <- NA
  }

  return(df)
}
