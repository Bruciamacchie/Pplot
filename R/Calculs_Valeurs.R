#' Calcul valeurs
#'
#' @description Calcul des valeurs de consommation et potentielle.
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_Valeurs <- function(df_Arbres, df_Quals, df_Prix, tauxR) {
  # df_Arbres = arbres
  # df_Quals = Quals
  # df_Prix = Prix

  PrixSup <- df_Prix |>
    rename(PUSup = PU)

  df_Arbres <- df_Arbres  |>
    left_join(df_Quals, by=c("Qual"="Nom")) |>
    mutate(Qual = ifelse(is.na(Qual), "C", Qual)) |>
    mutate(ClasseSup = Classe + 5) |>
    left_join(df_Prix, by=c("Essence","Classe","Reg1"="Qual")) |>
    left_join(PrixSup, by=c("Essence","ClasseSup"="Classe","Reg1"="Qual")) |>
    mutate(Vc = V * PU,
           tauxPU = log(PUSup/PU)/5,
           Gain = Vc*AccD*(tauxV + tauxPU),
           Vp = Gain / tauxR) |>
    dplyr::select(-c(Opérateurs,Miroir_Azimut:DiamLim))


  return(df_Arbres)
}
