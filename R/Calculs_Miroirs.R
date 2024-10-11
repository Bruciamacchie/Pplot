#' Traitement placettes miroirs
#'
#' @description Ajout d'arbres sur les placettes miroirs.
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'

Calculs_Miroirs <- function(df_Arbres,  df_Echantillonnages) {
  # df_Arbres = arbres
  # df_Echantillonnages = Echantillonnages

  tab <- df_Echantillonnages |>
    dplyr::select(NumForet:Strate,Coeff)

  df_Arbres1 <- df_Arbres  |>
    filter(!is.na(Miroir_Azimut) & !is.na(Miroir_Dist)) |>
    left_join(tab, by = join_by(NumForet, Cycle, Strate))

  if (dim(df_Arbres1)[1] > 0) {
    df_Arbres1 <- df_Arbres1 |>
      mutate(theta = (100-Azimut) %% 400,
             Miroir_theta = (100-Miroir_Azimut) %% 400,
             Miroir_Dist = Miroir_Dist * 2,
             c_X = Miroir_Dist * cos(Miroir_theta * pi / 200),
             c_Y = Miroir_Dist * sin(Miroir_theta * pi / 200),
             X = c_X + Dist * cos(theta * pi / 200),
             Y = c_Y + Dist * sin(theta * pi / 200),
             X = round(X, 2),
             Y = round(Y, 2),
             theta = ifelse(X != 0, atan(Y/X) * 200 / pi, 0),
             Dist = round(sqrt(X^2 + Y^2), 2),
             Azimut = Azimut +  400
             )

    df_Arbres1 <- df_Arbres1 |>
      dplyr::select(names(df_Arbres))

    df_Arbres = rbind(df_Arbres, df_Arbres1)
  }

  return(df_Arbres)
}
