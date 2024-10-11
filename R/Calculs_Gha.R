#' Calcul de Gha
#'
#' @description fonction de calcul de la surface terrière/ha (feuille Arbres).
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_Gha <- function(df_Arbres){

  df_Arbres <- df_Arbres  |>
    mutate(Gha = case_when(
      is.na(Coeff) ~ pi / 40000 * Diam ^ 2 * Nha,
      !is.na(Coeff) & Diam1 >= DiamLim ~ pi / 40000 * Diam1 ^ 2 *Nha,
      # !is.na(Coeff) & Diam1 < DiamLim & Diam >= DiamLim ~ pi / 40000 * Diam ^ 2 * Nha,
      !is.na(Coeff) ~ pi / 40000 * Diam ^ 2 * Nha))
return(df_Arbres)
}
