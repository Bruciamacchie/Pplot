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

Calculs_Valeurs <- function(df_Arbres, df_Quals, df_Prix) {
  # df_Arbres = Arbres
  # df_Quals = Quals
  # df_Prix = Prix

  df_Arbres1 <- df_Arbres  |>
    left_join(df_Quals, by=c("Qual"="Nom")) |>
    mutate(Qual = ifelse(is.na(Qual), "C", Qual)) |>
    left_join(df_Prix, by=c("Essence","Classe","Reg1"="Qual"))

  return(df_Arbres)
}
