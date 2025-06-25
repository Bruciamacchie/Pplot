#' Base DVF.
#'
#' @description Création de la base DVF pour un département.
#'
#' @return La fonction renvoie deux tables, DVFparcelles et DVFventes.
#'
#' @param dept = code département
#'
#' @import tidyverse
#' @import data.table
#'
#' @author Bruciamacchie Max
#' @export


DVFData <- function(dept) {
  fichiers <- list.files(path = "/Users/maxm1/pCloudSync/Cours/APT3/MicroProjetsR/2023/Sujet6/Data",
                         pattern = "\\.txt$", full.names = T)


  DVFventes    <- tibble()
  DVFparcelles <- tibble()

  for (i in 1:length(fichiers)) {
    print(paste("Traitement du fichier", fichiers[i], "........."))
    temp <- fread(fichiers[i],
                  select = c("No disposition","Date mutation","Nature mutation","Valeur fonciere","Code postal",
                             "Commune","Code departement", "Code commune", "Section","No plan","Type local",
                             "Surface reelle bati","Nature culture","Nature culture speciale","Surface terrain"),
                  dec=",") |>
      select_all(~gsub("\\s+|\\.", "_", .)) |>
      filter(Nature_mutation %in% c("Vente", "Adjudication")) |>
      filter(Code_postal <= 95000)  |>
      filter(!is.na(Nature_culture))  |>
      filter(!is.na(Valeur_fonciere))  |>
      filter(!is.na(Surface_terrain))

    names(temp) <- c("Disposition","Date","Nature","Valeur","CodePost","Commune","Dept","CodeCom",
                     "Section","Num","Type","Bati","Culture","NatCult","Surface")

    temp <- temp |>
      filter(Dept == dept)

    # ------------- STRUCTURATION BASES -------------
    print(paste("Creation variables complémentaires"))
    temp <- temp |>
      mutate(Année = as.integer(substr(Date, 7, 10))) |>
      mutate(INSEE = paste0(Dept,substr(CodePost, nchar(CodePost)-2, nchar(CodePost)))) |>
      mutate(ID = paste(INSEE, Nature, Date, Disposition, Valeur, sep="_"))  |>
      mutate(Foret = ifelse(Culture %in% c("B","BF","BM","BP","BR","BS","BT","L","LB"), 0, 1))

    ventes <- temp %>%
      dplyr::select(ID,Année,Disposition:Nature,INSEE,Commune,Dept,Valeur) %>%
      distinct()

    Parcelles <- temp %>%
      dplyr::select(Section:Foret)

    DVFventes    <- bind_rows(DVFventes, ventes)
    DVFparcelles <- bind_rows(DVFparcelles, Parcelles)
  }

  save(DVFventes, DVFparcelles, file = paste0("Tables/DFV",dept,".Rdata"))
}



