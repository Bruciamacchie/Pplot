#' Calcul données /ha
#'
#' @description Calcul des valeurs ramenées à l'hectare.
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'
#'

Calculs_Ha <- function(df_Arbres) {
  df_Arbres <- df_Arbres |>
    mutate(Vha = V * Nha,
           VIFNha = VIFN * Nha,
           VcHa = Vc * Nha,
           VpHa = Vp * Nha,
           GainHa = Gain * Nha)

  return(df_Arbres)
}
