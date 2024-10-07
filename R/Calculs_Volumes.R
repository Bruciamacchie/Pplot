#' Calcul des volumes.
#'
#' @description Calcul des volumes gestionnaires ou IFN à une ou deux entrées.
#'
#' @return La fonction complète le base arbres.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'
#'
Calculs_Volumes <- function(df_Arbres, df_Tarifs) {

  #--------------- Preambule ---------------
  df_Arbres <- df_Arbres  |>
    left_join(df_Tarifs, by=c("NumForet","Strate","Cycle","Essence")) |>
    mutate(V = case_when(TypeTarif=="SchR" ~ 5/70000*(8+NumTarif)*(Diam-5)*(Diam-10),
                         TypeTarif=="SchI" ~ 5/80000*(8+NumTarif)*(Diam-2.5)*(Diam-7.5),
                         TypeTarif=="SchL" ~ 5/90000*(8+NumTarif)*(Diam-5)*Diam,
                         TypeTarif=="SchTL" ~ 5/101250*(8+NumTarif)*Diam^2)) |>
    mutate(V = ifelse(V < 0, 0, V)) |>

    mutate(VSup = case_when(TypeTarif=="SchR" ~ 5/70000*(8+NumTarif)*(Diam)*(Diam-5),
                            TypeTarif=="SchI" ~ 5/80000*(8+NumTarif)*(Diam+2.5)*(Diam-2.5),
                            TypeTarif=="SchL" ~ 5/90000*(8+NumTarif)*(Diam+5)*Diam,
                            TypeTarif=="SchTL" ~ 5/101250*(8+NumTarif)*(Diam+5)^2)) |>
    mutate(VSup = ifelse(VSup < 0, 0, VSup)) |>
    # -------- Calcul du volume IFN
    mutate(VIFN = case_when(TypeTarifIFN=="SchR" ~ 5/70000*(8+NumTarifIFN)*(Diam-5)*(Diam-10),
                            TypeTarifIFN=="SchI" ~ 5/80000*(8+NumTarifIFN)*(Diam-2.5)*(Diam-7.5),
                            TypeTarifIFN=="SchL" ~ 5/90000*(8+NumTarifIFN)*(Diam-5)*Diam,
                            TypeTarifIFN=="SchTL" ~ 5/101250*(8+NumTarifIFN)*Diam^2))  |>
    mutate(VIFN = ifelse(VIFN < 0, 0, VIFN))

  # ---------- Tarif CHAUDE ------------
  if("Chaudé" %in% unique(df_Arbres$TypeTarif)) {
    data("decChaude")
    df2 <- decChaude |>
      pivot_longer(cols = Ch1:Ch20, names_to ="NumTarif", values_to = "delta") |>
      mutate(NumTarif = as.numeric(gsub("Ch", "", NumTarif)))

    df_Arbres <- df_Arbres |>
      left_join(df2, by = join_by(Classe, NumTarif)) |>
      mutate(delta = ifelse(is.na(delta), 2, delta)) |>
      mutate(V = ifelse(TypeTarif == "Chaudé", pi/40000*(Diam-delta*(Haut/2-1.3))^2*Haut, V)) |>
      mutate(VSup = ifelse(TypeTarif == "Chaudé", pi/40000*(Diam+5-delta*(Haut/2-1.3))^2*Haut, VSup))
  }

  df_Arbres <- df_Arbres |>
    mutate(tauxV = ifelse(V==0,0,log(VSup/V)/5)) |>
    dplyr::select(-c(TypeTarif,NumTarif,TypeTarifIFN,NumTarifIFN,VSup, delta))

  return(df_Arbres)
}
