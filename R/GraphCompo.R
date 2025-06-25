#' Composition en essence
#'
#' @description fonction de calcul du poids (feuille Arbres).
#'
#' @return Graphique ggplot.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export

GraphCompo <- function(df1, df2) {
  Palette_Essence <- df2 |>
    filter(EssReg %in% unique(df1$EssReg)) |>
    dplyr::distinct(EssReg, Couleur)

  OrdreEssence <- df1 |>
    ungroup() |>
    filter(Cycle == max(Cycle)) |>
    dplyr::select(EssReg, Gha) |>
    group_by(EssReg) |>
    summarise(Gha = sum(Gha)) |>
    arrange(desc(Gha))

  t_Radar <- df1 |>
    dplyr::select(EssReg, Nha,Gha,Vha,VcHa,VpHa) |>
    arrange(Groupe, Cycle, -Gha) |>
    rename(N=Nha,
           G=Gha,
           V=Vha,
           Vc=VcHa,
           Vp=VpHa)

  t_Radar.m <- t_Radar |>
    pivot_longer(cols = c(N,G,V,Vc,Vp)) |>
    group_by(Groupe, Cycle, name) |>
    mutate(Tot = sum(value, na.rm=T),
           Ratio=value/Tot,
           RatioCum=cumsum(value/Tot)) |>
    mutate(label = NA,
           label = case_when(
             name =="N" ~ paste0(round(Ratio,3)*100," %"),
             name =="G" ~ paste0(round(value,1)," m2/ha"),
             name =="V" ~ paste0(round(value,1)," m3/ha"),
             name =="Vc" ~ paste0(round(value,0)," euros/ha"),
             name =="Vp" ~ paste0(round(value,0)," euros/ha")
           )) |>
    left_join(Palette_Essence, by = join_by(EssReg)) |>
    arrange(name, EssReg)

  t_Radar_Text <- t_Radar.m |>
    group_by(name) |>
    summarise(value=mean(value,na.rm=T)) |>
    ungroup()

  g <- ggplot() +
    geom_bar(t_Radar.m, mapping=aes(x=name, y=value, fill=EssReg),
             stat='identity', position='fill') +
    facet_grid(Cycle ~ Groupe) +
    geom_label(t_Radar_Text, mapping=aes(x=name,
                                         y=0.5,
                                         label=name),
               label.padding = unit(0.15, "lines"),
               label.r=unit(0, "lines"),
               size=2.5, fill="gray70", alpha=0.5,
               fontface="bold") +
    coord_polar(theta = "y") +
    scale_x_discrete("") +
    scale_y_continuous(breaks=seq(0,1,0.1),
                       name="Part relative des essences",
                       labels = percent) +
    # scale_fill_manual(values=Palette_Essence$Couleur) +
    guides(fill=guide_legend(ncol=ifelse(length(unique(t_Radar.m$EssReg)) > 8,
                                         round(length(unique(t_Radar.m$EssReg))/6,0),1),
                             bycol=TRUE,
                             reverse=T)) +
    theme_bw() + theme(title=element_text(face='plain'),
                       axis.text.y  = element_blank(),
                       axis.text.x = element_text(size = 7),
                       axis.ticks = element_blank(),
                       axis.title.x=element_text(face='plain',size=9),
                       legend.text = element_text(size = 7),
                       plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
                       panel.grid.major = element_blank(),
                       panel.border=element_blank()) +
    labs(fill="") +
    guides(fill=guide_legend(ncol=1,byrow=TRUE))

  return(g)
}


