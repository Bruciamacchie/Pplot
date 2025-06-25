#' Calcul bois mort au sol
#'
#' @description Calcul du bois mort au sol par cercle.
#'
#' @return Volume par hectare de bois mort au sol.
#'
#' @import tidyverse
#'
#' @author Demets Valentin
#' @export
#'

Calculs_bms_cercle <- function(
    df = NULL, code_essreg = NULL) {
  df <- if (dim(df)[1] > 0) {
      df |>
        mutate(
          DiamIni = ifelse(is.na(DiamIni), 0, DiamIni),
          DiamMed = ifelse(is.na(DiamMed), 0, DiamMed),
          DiamFin = ifelse(is.na(DiamFin), 0, DiamFin),
          Vha = 0,
          Classe = 0,
          # ---- formule de Huber
          Vha = ifelse(
            (DiamIni+ DiamFin) == 0,
            pi / 40000 * DiamMed * Longueur * 10000 / pi / Rayon ^ 2,
            Vha
          ),
          Classe = ifelse(
            (DiamIni + DiamFin) == 0,
            floor(DiamMed / 5 + 0.5) * 5,
            Classe
          ),
          StadeE = floor(Stade / 10),
          StadeD = Stade - StadeE * 10,
          # ---- formule de Smalian
          Vha = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed == 0,
            pi / 80000 * (DiamIni ^ 2 + DiamFin ^ 2) *
              Longueur * 10000 / pi / Rayon ^ 2,
            Vha
          ),
          Classe = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed == 0,
            floor((DiamIni + DiamFin) / 2 / 5 + 0.5) * 5,
            Classe
          ),
          # ---- formule de Newton
          Vha = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed != 0,
            pi / 240000 * (DiamIni ^ 2 + DiamFin ^ 2 + 4 * DiamMed ^ 2) *
              Longueur * 10000 / (pi * Rayon ^ 2),
            Vha
          ),
          Classe = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed != 0,
            floor((DiamIni + DiamFin + DiamMed) / 3 / 5 + 0.5) * 5,
            Classe
          )
        ) |>
        mutate(
          Cat = cut(
            Classe,
            breaks = c(0, 17.5, 27.5, 47.5, 67.5, 500),
            labels = c("PER", "PB", "BM", "GB", "TGB"),
            include.lowest = T,
            right = F
          ),
          Cat = as.character(Cat)
        ) |>
        left_join(code_essreg, by = c("NumForet", "Essence")) %>%
        # TODO : add find_ESSREG ? sur autres tables aussi
        mutate(EssReg = as.character(EssReg)) %>%
        select(
          NumForet, NumPlac, Cycle, Essence, EssReg,
          DiamIni, DiamMed, DiamFin,
          Classe, Cat, Stade, StadeD, StadeE, Vha
        )
    } else {
      BMSsup30 <-
        data.frame(
          NumForet = numeric(), NumPlac = character(),
          Cycle = numeric(),
          Essence = character(), EssReg = character(),
          Diam = numeric(), Classe = numeric(),
          StadeD = numeric(), StadeE = numeric(), Vha = numeric(),
          stringsAsFactors = F
        )
    }

  return(df)
}
