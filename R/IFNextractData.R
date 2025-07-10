#' Import IFN data
#'
#' @description Import des données brutes de l'IFN.
#'
#' @return La fonction renvoie toutes les tables CSV de l'IFN.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export
#'


# adresse = "https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2005_2023.zip"
# IFNdata <- IFNextractData(adresse)
# arbres <- IFNdata$ARBRE


IFNextractData <- function(adress) {

  td <- tempdir()
  tf <- tempfile(tmpdir=td, fileext=".zip")

  download.file(adress, tf)
  file_names <- unzip(tf, list=TRUE)
  t1 <- file_names[grep('.csv', file_names$Name), ]
  tab <- lapply(t1$Name, function(x) import(file.path(td, x)))
  names(tab) <- str_split(t1$Name, "\\.", simplify=T)[,1]

  unlink(td)

  return(tab)
}


