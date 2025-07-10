#' Calcul du bois mort sur pied
#'
#' @description Calcul du bois mort sur pied par échantillonnage linéaire.
#'
#'
#' @return table BMPpcqm
#'
#' @import tidyverse
#'
#' @author Max Bruciamacchie
#' @export
#'

Calcul_BMP_pcqm <- function(df, seuilVisib=25) {
  # ----------- Import -------
  if (dim(df)[1] <= 0) stop(print("Votre tableau est vide"))

  listeNames <- c("NumForet","NumPlac","Cycle","Quart","Essence","Azimut","Dist","Diam","Type","Haut","Stade")

  if (!all(listeNames %in% names(df))) stop(print(paste("Le tableau PCQM ne contient pas les colonnes", listeNames)))

  df <- df |>
    dplyr::select(listeNames) |>
    mutate(QuartCor = floor(Azimut/100)+1) |>
    mutate(Quart = ifelse(QuartCor > 4, QuartCor - 4, QuartCor)) |>
    dplyr::select(-QuartCor) |>
    filter(!is.na(Dist))

  # ----------- Distribution des distances -------
  g <- ggplot(df, aes(x = Dist)) +
    geom_histogram(aes(y = after_stat(density)),
                   colour = 1, fill = "grey80", binwidth=1) +
    geom_density() +
    theme_bw() + labs(y="Probabilité", x="Distance (m)")

  seuilDist = quantile(df$Dist, probs=0.95)

  # ----------- Poids d'une tige -------
  poids <- df |>
    group_by(NumForet, Cycle, NumPlac) |>
    summarise(Nbre = n(),
              Dist2 = sum(Dist^2)) |>
    mutate(Vides = 4 - Nbre,
           Dist2 = Dist2 + Vides * seuilDist^2,
           Nha = 10000*3/pi/Dist2)

  df <- df |>
    left_join(poids, by = join_by(NumForet, NumPlac, Cycle)) |>
    dplyr::select(-Nbre, -Dist2, -Vides) |>
    mutate(Gha =  pi/40000*Diam^2 * Nha) |>
    mutate(Vha =NA,
           Vha = ifelse(Type == "S", pi/40000*Diam^2*Haut, Vha),
           Vha = ifelse(Type == "V", pi/40000*(Diam-1.5*(Haut/2-1.3))^2*Haut, Vha),
           Vha = ifelse(Type == "A", 5/90000*(8+7)*(Diam-5)*Diam, Vha),
           Vha = Vha * Nha
           )

  return(df)
}


