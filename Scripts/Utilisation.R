librarian::shelf(tidyverse,sf, readxl,Pplot, tmap)



############## VERIFICATION #################
system.file()
fich = "~/Downloads/guRnaud/dossiers/PNRVN/data/excel/inventaires/PNRVN_2025.xlsx"
Verif_structure(fich)
Verif_data(fich)

############## Import #################
gf_Xls2Rdata(dirname(fich), fich)


############## Traitements #################

# --------fichier arbres
Arbres <- gf_Calculs(fich, 0.0225)

# --------fichier Reges


#--------------- Calculs BMS ---------------
BMS <- Calculs_BMS_lineaire(BMSLineaires)

############## Evolution ##############

# DeltaArbres <- AcctArbres(Arbres)

Accroissements_Plac <- gf_Evol(Arbres)
save(Accroissements_Plac, file = "Tables/AcctPlac.Rdata")

# t1 <- Arbres |>
#   filter(Essence == "meriser")
############## Agregation arbres #################
gf_AgregArbres()

############## Agregation placettes #################
gf_AgregPlac()



tm_shape(perim) +
  tm_borders(alpha=.4) +
  tm_shape(Accroissements_Plac) +
  tm_dots(size = "Acct", palette = "Reds", style = "quantile", title = "Price Paid (?)") +
  tm_facets(by="name", free.scales.symbol.size = T)


TauxSauv <- read_excel("/Users/maxm1/pCloudSync/Packages/Pplot/Test/Sauvegarde.xlsx")
usethis::use_data(TauxSauv, overwrite = T)
save(TauxSauv, file="/Users/maxm1/pCloudSync/Packages/Pplot/data/TauxSauv.rda", overwrite = T)


usethis::use_data(CodeEcolos, overwrite = T)


writexl::write_xlsx(Arbres, "Arbres.xlsx")

