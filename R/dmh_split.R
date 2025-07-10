#' Décomposition des codes de DMH
#' @description La fonction intervient dans la fonction calculs_dmh et permet de créer la table Codes
#' @param df = table arbres
#' @param list = liste des codes DMH rencontrés - à préciser
#' @import tidyverse
#' @export
#'



dmh_split <- function(df){
  df <- df |>
    dplyr::select(NumForet:Essence,Cycle,Classe,Stade,CodeEcolo,Nha,Gha)

  df <- df |>
    dplyr::mutate(Codes = ifelse(Cycle == 1,
                                 str_split(CodeEcolo, " "),
                                 str_split(CodeEcolo, ", "))) |>
    dplyr::select(NumForet:Essence,Cycle,Codes,Nha,Gha) |>
    dplyr::filter(!is.na(Codes)) |>
    unnest_longer(Codes) |>
    dplyr::mutate(Codes = str_squish(Codes))

  return(df)
}


