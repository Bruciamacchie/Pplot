#' Valeur des forêts
#'
#' @description Les valeurs des forêts sont issues de la base  DVF (Demandes de valeurs foncières)
#' du site data.gouv.fr. Les deux département de l'Alsace ainsi que la Moselle ne figurent pas dans la
#' base de données. Les DOM-TOM ont été retirés de l'analyse.
#'
#' @param an = année. Ce paramètre n'accepte actuellement que les années 2016, 2017 et 2018.
#' @param minSurf = seuil minimal de surface en ha. Par défault minSurf = 1 ha.
#' @param maxPrix = seuil maximal de prix à l'hectare. Par défault maxPrix = 50000 euro/ha.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#'
#' Evol <- data.frame()
#' Detail <- data.frame()
#' for (i in c(2018:2016)) {
#'   res  <- ValeurVenale(i)
#'   Evol <- rbind(Evol, as.data.frame(res$tab))
#'   Detail <- rbind(Detail, as.data.frame(res$Detail))
#' }
#'
#' textes <- Evol %>% filter(Année == 2018)
#' ggplot(Evol, aes(x=Nature, y=Prix, color=factor(Année))) +
#'   geom_point() + geom_text(data=textes, aes(label=round(Prix,0)), size=4, hjust=1.2) +
#'   theme_bw() + labs(color="")
#'
#' ggplot(Detail, aes(x=Nature, y=Prix, fill=factor(Année), color=factor(Année))) +
#' geom_point(position = position_jitterdodge()) +
#'   geom_boxplot(alpha=0.4) +
#'   geom_text(data=textes, aes(label=round(Prix,0)), size=3, hjust=-1.8) +
#'   theme_bw() + labs(fill="", color="", y="Prix (euro/ha)")
#' }
#
#' @export

DVFvalues <- function(minSurf=1, maxPrix=50000) {
  # ------------- Pouvoir d'achat ------------
  data("INSEEpa")
  euro=6.55957
  tab <- INSEEpa |>
    mutate(Coefft=1,
           Coefft = ifelse(Année < 2002, 1/euro, Coefft),
           Coefft = ifelse(Année < 1960, 1/euro/100, Coefft),
           Infla = max(Infla)/Infla,
           Coefft = Coefft * Infla) |>
    dplyr::select(Année, Coefft)

    # ------------- Import ------------
  load("Tables/DFV72.Rdata")

  DVFventes <- DVFventes |>
    filter(Nature == "Vente")

  ventesParcelles <- DVFparcelles |>
    group_by(ID, Foret) |>
    summarise(Foret = sum(Foret),
              Surface = sum(Surface)/10000) |>
    filter(Foret == 0) |>
    filter(Surface >=1)

  DVFparcellesForet <- DVFparcelles |>
    filter(Foret == 0) |>
    filter(ID %in% ventesParcelles$ID) |>
    group_by(ID, Culture) |>
    summarise(Surface = sum(Surface)) |>
    group_by(ID) |>
    mutate(Pourc = Surface/sum(Surface))

  purs <- DVFparcellesForet |>
    filter(Pourc >=1)

  Nbre <- purs |>
    group_by(Culture) |>
    summarise(Freq = n())

  DVFventes1 <- DVFventes |>
    filter(ID %in% purs$ID) |>
    left_join(purs, by = join_by(ID)) |>
    mutate(Prix = Valeur / Surface*10000) |>
    left_join(tab, by = join_by(Année)) |>
    mutate(Prix = Prix * Coefft)

  Fonds <- DVFventes1 |>
    filter(Culture == "L")


  ggplot(DVFventes1, aes(x=Culture, y=Prix, color=Année)) +
    geom_jitter(width=0.2) +
    theme_bw()+ ylim(0, 100000)

  ggplot(DVFventes1, aes(x=Culture, y=Prix)) +
    geom_boxplot() +
    theme_bw()+ ylim(0, 5000)

    return(out)
}

communes <- st_read("/Users/maxm1/pCloud Drive/Bureau/GeoData/Limites/Administratifs/COMMUNE.shp") |>
  filter(INSEE_DEP == "72") |>
  left_join()

CL <- st_read("/Users/maxm1/pCloud Drive/Bureau/GeoData/Limites/Administratifs/CHEF_LIEU.shp") |>
  mutate(Dept = str_sub(INSEE_COM, 1,2)) |>
  filter(Dept == "72")



par(mar=c(0,0,0,0))
ggplot +
  geom_sf(data=communes, fill=)
plot(st_geometry(communes), lwd=0.3)
