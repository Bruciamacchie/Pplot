#' Calcul PCQM
#' @description Calcul du bois mort sur pied par échantillonnage linéaire.
#'
#'
#' @return table BMPpcqm
#'
#' @import tidyverse
#'
#' @author Demets Valentin
#' @export
#'

# df = PCQM
Calcul_pcqm <- function(df, pop="BMP", seuilVisib=25) {
  # ----------- Import -------
  df <- df |> filter(Population == pop)

  if (dim(df)[1] <= 0) stop(print("Votre tableau est vide"))

  listeNames <- c("NumForet","NumPlac","Cycle","Quart","Essence","Azimut","Dist","Diam", "Stade")

  if (!all(listeNames %in% names(df))) stop(print(paste("Le tableau PCQM ne contient pas les colonnes", listeNames)))

  df1 <- df |>
    dplyr::select(listeNames) |>
    mutate(QuartCor = floor(Azimut/100)+1) |>
    mutate(Quart = ifelse(QuartCor > 4, QuartCor - 4, QuartCor)) |>
    dplyr::select(-QuartCor) |>
    filter(!is.na(Dist)) |>
    arrange(NumForet,Cycle,NumPlac,Quart,Dist) |>
    group_by(NumForet,Cycle,NumPlac,Quart,Type) |>
    slice(1)

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
    mutate(Gha =  pi/40000*Diam^2 * Nha)

  return(df)
}
