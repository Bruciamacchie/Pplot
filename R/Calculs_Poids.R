#' Calcul de Nha (Poids)
#'
#' @description fonction de calcul du poids (feuille Arbres).
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_Poids <- function(df_Arbres, df_Echantillonnages) {
  # df_Echantillonnages = Echantillonnages
  # df_Arbres <- arbres

  tab <- df_Echantillonnages |>
    dplyr::select(NumForet:Strate,DiamLim1:DiamLim)

  # ------------ tiges vivantes -----------
  df_Arbres <- df_Arbres  |>
    left_join(tab, by=c("NumForet","Cycle","Strate")) |>
    mutate(Nha = case_when(
      # ------------ Cercles
      is.na(Coeff) & is.na(Dist) & Diam >= DiamLim1 & Diam < DiamLim2 ~
        10000 / pi / Rayon1 ^ 2,
      is.na(Coeff) & !is.na(Dist) & Diam >= DiamLim1 & Diam < DiamLim2 & Dist <= Rayon1 ~
        10000 / pi / Rayon1 ^ 2,
      is.na(Coeff) & Diam >= DiamLim2 & Diam < DiamLim3 & Dist <= Rayon2 ~
        10000 / pi / Rayon2 ^ 2,
      is.na(Coeff) & Diam >= DiamLim3 & Dist <= Rayon3 ~
        10000 / pi / Rayon3 ^ 2,
      # ------------ Angles
      !is.na(Coeff) & Diam1 >= DiamLim & Diam1 >= Dist * Coeff ~
        10 ^ 8 * Coeff ^ 2 / pi / Diam1 ^ 2,
      !is.na(Coeff) & Diam >= DiamLim1 & Diam < DiamLim & Dist <= Rayon1 ~
        10000 / pi / Rayon1 ^ 2,
      # ------------ BMP
      !is.na(Type) & Diam >= 30 & Dist <= 20 ~ 10000 / pi / 20 ^ 2,
      !is.na(Type) & Diam < 30 & Dist <= 10 ~ 10000 / pi / 10 ^ 2,
      .default = 0
    ))

    return(df_Arbres)
  }
