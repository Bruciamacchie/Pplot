#' Risque arbres.
#'
#' @description Prise en compte des risques à l'échelle de l'arbre
#'
#' @return La fonction construit la table Arbres.
#'
#' @param df_Arbres = Table Arbres
#' @param df_Sauv = Valeurs de sauvegarde par essence et classe de diamètre
#' @param Temps = Temps de retour de l'aléa
#' @param Rot = Temps de rotation des coupes
#' @param tauxR = taux d'actualisation
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_Risques <- function (df_Arbres, Temps=150, Rot=8, tauxR=0.0225) {
  # df_Arbres = Arbres

  data("TauxSauv")

  prob = pexp(1/Temps, Rot)

  df_Arbres <- df_Arbres |>
    left_join(TauxSauv, by = join_by(Essence, Qual, Classe)) |>
    mutate(Sauv = ifelse(is.na(Sauv), 0.1, Sauv)) |>
    mutate(taux = ifelse(Vc == 0, 0, Gain / Vc)) |>
    mutate(tauxRisq = ((1-prob)*(1+taux*Rot)+prob*Sauv*(1+taux*Rot/2)-1)/Rot) |>
    mutate(GainHaRisq = VcHa * tauxRisq) |>
    mutate(VpHaRisq = GainHaRisq/tauxR)

  return(df_Arbres)
}
