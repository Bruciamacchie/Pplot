####################### Fonction PCQM ###################
CalculPoidsPCQM <- function(df, seuilVisib=25) {
  
  # ----------- Import -------
  if (dim(df)[1] <= 0) stop(print("Votre tableau est vide"))
  
  listeNames <- c("NumForet","NumPlac","Cycle","Quart","Essence","Azimut","Dist","Diam")
  
  if (!all(listeNames %in% names(df))) stop(print(paste("Le tableau PCQM ne contient toutes les colonnes", listeNames)))
  
  # df <- df |> 
  #   dplyr::select(listeNames)
  
  # ----------- Distribution des distances -------
  g <- ggplot(df, aes(x = Dist)) + 
    geom_histogram(aes(y = after_stat(density)),
                   colour = 1, fill = "grey80", binwidth=1) +
    geom_density() +
    theme_bw() + labs(y="Probabilité", x="Distance (m)")
  
  seuilDist = quantile(df$Dist, probs=0.99)
  
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

####################### Fonction quantile ###################
TransQuantile <- function(s, q=seq(0,1,0.1)){
  exs = stack(lapply(1:nlayers(s), function(i){extremesOut(s[[i]], q=seq(0,1,0.1))}))
  
}