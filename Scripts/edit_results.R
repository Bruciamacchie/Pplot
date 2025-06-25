# -- activate/install libraries
# install.packages("easypackages") # if 1st use
library(easypackages)
packages(
  "stringr", "openxlsx", "rmarkdown", "tools",
  "tidyr", "dplyr", "gWidgets2", "gWidgets2tcltk", "knitr", "maptools",
  "xtable", "ggplot2", "ggrepel", "ggthemes", "scales", "gridExtra",
  "rgeos", "rgdal", "gdata", "grid", "fmsb", "rlang"
)

# -- clear env
rm(list = ls())

# -- set app_dir
app_dir <- "/Users/Valentin/Travail/Outils/Inventaire_PP/scripts/PermGF3"

# -- chargement des scripts
# chargement du script des fonctions annexes
source(file.path(app_dir, "scripts/annexes.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 1 : import des donnnées
source(file.path(app_dir, "scripts/gf1_Xls2Rdata.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 2 : vérification des donnnées
source(file.path(app_dir, "scripts/gf2_Verif.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 3 : calcul des variables de résultats, par arbre
source(file.path(app_dir, "scripts/gf3_Calculs.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 4 : calcul des variables de résultats, par placette
source(file.path(app_dir, "scripts/gf4_AgregArbres-new.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 5 : calcul des variables de résultats, par ensemble
source(file.path(app_dir, "scripts/gf5_AgregPlacettes.R"), encoding = 'UTF-8', echo = TRUE)
# chargement du script job 10 : édition des résultats au format excel
source(file.path(app_dir, "scripts/gf10_Tables2Xls_2022.R"), encoding = 'UTF-8', echo = TRUE)

# -- create translator tool
translator <- shiny.i18n::Translator$new(translation_json_path = file.path(app_dir, "translations/translation.json"))
# translator$set_translation_language("English")
translator$set_translation_language("Français")
# i18n reactive function
i18n <- function() {
  translator
}



# -- set wd
wd <- "/Users/Valentin/Travail/Outils/Inventaire_PP"
setwd(wd)
### liste des tables nécessaires à l'édition du livret
# tables_list <- c(
#   "gfForetBMP_", "gfForetBMP_Classe", "gfForetBMP_ClasseStadeD",
#   "gfForetBMP_StadeD", "gfForetBMP_StadeE",
#   "gfForetBMP_ClasseType",
#   "gfForetBMP_StadeDStadeE", "gfForetBMS_", "gfForetBMS_Classe",
#   "gfForetBMS_StadeDStadeE",
#   "gfForetBMS_ClasseStadeD", "gfForetBMS_StadeD", "gfForetBMS_StadeE",
# 
#   "gfForetBM_Essence", "gfForetBM_", "gfForetBM_Classe",
#   "gfForetBM_EssReg", "gfForetBM_EssRegClasse",
#   "gfForetBM_StadeD", "gfForetBM_StadeE",
# 
#   "gfForetCodes_CatCodeEcolo", "gfForetCodes_CodeEcolo",
# 
#   "gfForetFpied_", "gfForetFpied_EssReg", "gfForetFpied_EssRegCat", "gfForetFpied_EssRegClasse",
#   "gfForetFpied_Cat", "gfForetFpied_Classe", "gfForetFpied_ClasseQual", "gfForetFpied_ClasseReg1",
#   "gfForetFpied_Reg1", "gfForetFpied_Reg2", "gfForetFpied_CatReg1",
#   "gfForetFpied_Essence", "gfForetFpied_EssenceClasse",
#   "gfForetFpied_EssenceClasseReg1", "gfForetFpied_EssRegReg1",
#   "gfForetFpied_Coupe", "gfForetFpied_EssRegCatCoupe", "gfForetFpied_CatCoupe",
#   "gfForetFpied_CatReg2Coupe", "gfForetFpied_EssRegCoupe",
#   "gfForetFpied_Reg2Coupe", "gfForetFpied_Reg2Coupe",
#   "gfForetFpied_ClasseCodeEcolo", "gfForetFpied_CatCodeEcolo", "gfForetFpied_CodeEcolo",
#   "gfForetFpied_Reg2CodeEcolo", "gfForetFpied_EssenceCodeEcolo",
#   "gfForetFpied_CatReg2CodeEcolo",
# 
#   "gfForetPer_Essence", "gfForetFpied_EssenceCat",
#   "gfForetPer_Reg2",
# 
#   "gfForetFpied_CatCodeEcolo", "gfForetFpied_", "gfForetFpied_Cat",
#   "gfForetFpied_CatReg1", "gfForetFpied_Classe", "gfForetFpied_ClasseQual", "gfForetFpied_ClasseReg1",
#   "gfForetFpied_Essence", "gfForetFpied_EssenceCat", "gfForetFpied_EssRegCat",
#   "gfForetFpied_Reg2", "gfForetPer_Classe", "gfForetFpied_ClasseCodeEcolo",
#   "gfForetFpied_Coupe",
# 
#   "gfForetPer_ClasseReg1", "gfForetPer_EssRegClasse", "gfPPTetrasPer_EssReg",
#   "gfForetPer_",
#   "gfForetRege_Essence", "gfForetRege_EssReg", "gfForetRege_",
#   "gfForetRege_EssenceRejet",
# 
#   "gfForetTaillis_", "gfForetTaillis_Essence",
#   "gfForetTaillis_EssReg", "gfForetTaillis_Classe", "gfForetTaillis_EssRegClasse",
# 
#   "gfPlaBMP_", "gfPlaBMS_", "gfPlaFpied_", "gfPlaFpied_", "gfPlaFpied_EssReg",
#   "gfPlaPerches_EssRegClasseReg1", "gfPlaFpied_Cat",
#   "gfPlaRege_EssReg", "gfPlaTaillis_", "gfPlaTaillis_EssReg",
# 
#   "gfPlaFpied_EssenceCatRep\u00E9r\u00E9", "gfPlaBMP_EssenceCat",
#   "gfForetRege_Essence",
#   "gfForetFpied_EssenceReg1", "gfForetFpied_EssenceCatReg1",
#   "gfForetFpied_EssenceCatRep\u00E9r\u00E9PerchoirAbri",
#   "gfForetFpied_EssenceCatPerchoirAbri",
#   "gfForetFpied_EssenceClassePerchoirAbri",
#   "gfForetFpied_EssenceClasseReg1", "gfForetFpied_Reg1",
#   "gfForetFpied_EssRegReg1", "gfForetFpied_EssenceReg1",
#   "gfForetFpied_EssenceClasse",
# 
#   "gfPlaFpied_EssRegCatRep\u00E9r\u00E9", "gfPlaBMP_EssRegCat",
#   "gfForetRege_EssReg",
#   "gfForetFpied_EssRegReg1", "gfForetFpied_EssRegCatReg1",
#   "gfForetFpied_EssRegCatRep\u00E9r\u00E9PerchoirAbri",
#   "gfForetFpied_EssRegCatPerchoirAbri",
#   "gfForetFpied_EssRegClassePerchoirAbri",
#   "gfForetFpied_EssRegClasseReg1", "gfForetFpied_Reg1",
#   "gfForetFpied_EssRegReg1", "gfForetFpied_EssRegReg1",
#   "gfForetFpied_EssRegClasse",
# 
#   # -- éléments manquants suite à la MAJ du job10 :
#   "gfForetBMS_EssenceClasseStadeDStadeE",
#   "gfForetBMP_EssenceClasseStadeDStadeEType",
#   "gfForetFpied_Qual",
#   "gfForetFpied_EssenceQual",
#   "gfForetFpied_Qual",
#   "gfForetFpied_EssenceQual",
#   "gfForetPer_Reg1",
#   "gfForetPer_EssRegReg1",
#   "gfForetRege_EssRegRejet",
#   "gfForetRege_Rejet",
#   "gfForetBM_StadeDStadeE",
# 
#   "gfForetCarbone_", "gfForetCarbone_Essence", "gfForetCarbone_Cat", "gfForetCarbone_Reg1",
#   "gfForetCarbone_Lifetime", "gfForetCarbone_EssenceLifetime", "gfForetCarbone_CatLifetime", "gfForetCarbone_Reg1Lifetime",
#   "gfForetBMS_EssenceCat", "gfForetBMS_Essence", "gfForetBMP_Essence", "gfForetBMS_Cat", "gfForetBMP_Cat",
#   "gfForetExploites_", "gfForetChablis_"
# )
# tables_list <- sort(unique(tables_list))
# save(tables_list, file = "tables/report_tables_list.Rdata")
###
load("tables/report_tables_list.Rdata")

# construction de la table des combinaisons
results_by_plot_to_get <- build_combination_table(tables_list)
results_by_stand_to_get <- data.frame(
  V1 = "Foret",
  V2 = NA,
  stringsAsFactors = F
)

forest_list <- c(23)
for (forest in forest_list) {
  # forest <- 22 # debug
  # arguments shiny
  rv <- list()
  # db <- new.env()
  db = global_env() # debug
  tables <- load("tables/gfDonneesBrutes.Rdata", envir = db)
  with(db, {
    rv <- list(
      repGF = "/Users/Valentin/Travail/Outils/Inventaire_PP", # wd
      # lang = "Deutsch", # choisir la langue du fichier d'import
      lang = "Français",
      # lang = "English",
      # lang = "Deutsch",
      forest_num = forest
    )
    # -- gestion des noms et num de la forêt
    # TODO : faire le tri des éléments à rendre vraiment réactifs
    # rv$forest_num <- as.numeric(str_sub(rv$forest, 1, str_locate(rv$forest, "-")[, 1] - 1))
    # print(paste0("rv$repGF = ", rv$repGF)) # debug
    rv$forest_name <-
      with(db[["Forets"]], Nom[match(rv$forest_num, NumForet)])
    rv$forest <- paste0(rv$forest_num, "-", clean_names(rv$forest_name))
    # browser()
    # -- arguments relatifs à la forêt
    rv$last_cycle <-
      with(db[["Cycles"]], max(Cycle[NumForet == rv$forest_num], na.rm = T))
    rv$last_year <-
      with(db[["Cycles"]], Annee[NumForet == rv$forest_num & Cycle == rv$last_cycle])
    
    if (length(rv$last_year) > 1) {
      stop("Correction du classeur administrateur nécessaire : il y a 2 années identiques renseignées dans la feuille Cycles")
    }
    
    # -- création du dossier de sortie
    rv$output_dir <- file.path("out", clean_names(rv$forest), "livret_GF")
    
    # -- définition des arguments nécessaires au knit
    rv$rep_pdf <- file.path(rv$repGF, rv$output_dir)
    rv$rep_logos <- file.path(rv$repGF, "data/images/logos/")
    rv$rep_figures <- file.path(rv$rep_pdf, "figures/")
    rv$rep_sav <- dirname(rv$rep_pdf)
    
    # chemin du template (absolute path)
    # rv$template_path <- file.path(wd, "template/afi_Livret_2020_shiny_work.Rnw")
    rv$template_path <- file.path(rv$repGF, "scripts/PermGF3/scripts/gf_Carnet_2022.Rnw")
    # nom de la sortie en .tex
    rv$output_filename <- paste0(rv$forest_num, "_livret_GF-", rv$last_year, ".tex")
    # rv$output_filename <- paste0(rv$forest_num, "_annexe_carbone_AFI_", rv$last_year, ".tex")
    
    rv <<- rv
    
    # -- filtre des tables liées à l'inventaire en fonction du numéro de forêt sélectionné
    filter_by_forest(tables, forest_list = rv$forest, cycle = rv$last_cycle)
  })
  
  ##### Job 4 : calcul des résultats par arbre #####
  wd = rv$repGF
  output_dir = rv$rep_sav
  forest = rv$forest
  last_cycle = rv$last_cycle
  TauxR = 0.03
  
  gf_Calculs(
    wd = rv$repGF,
    output_dir = rv$rep_sav,
    # output_dir = rv$repGF,
    forest = rv$forest,
    # forest = NULL,
    last_cycle = rv$last_cycle,
    # last_cycle = NULL,
    complete_progress = complete_progress,
    i18n = i18n
  )
  
  ##### Job 5 : agrégation des résultats par placettes #####
  wd = rv$repGF
  output_dir = rv$rep_sav
  forest = rv$forest
  last_cycle = rv$last_cycle
  combination_table = results_by_plot_to_get
  
  gf_AgregArbres(
    wd = rv$repGF,
    output_dir = rv$rep_sav,
    combination_table = results_by_plot_to_get,
    forest = rv$forest, last_cycle = rv$last_cycle,
    complete_progress = complete_progress,
    i18n = i18n
  )
  
  ##### Job 6 : agrégation des résultats par forêt #####
  wd = rv$repGF
  output_dir = rv$rep_sav
  forest = rv$forest
  last_cycle = rv$last_cycle
  combination_table = results_by_stand_to_get
  
  gf_AgregPlacettes(
    wd = rv$repGF,
    output_dir = rv$rep_sav,
    combination_table = results_by_stand_to_get,
    forest = rv$forest, last_cycle = rv$last_cycle,
    complete_progress = complete_progress,
    i18n = i18n
  )

  ###### Job 7 : édition du livret d'analyse #####
  # TODO : filtrer les tables (avec "filter_by_forest" ?)
  wd <- function() {rv$repGF} # define wd() pour lancement manuel
  # rv$lang <- "Deutsch"
  dir.create(rv$output_dir, showWarnings = F, recursive = T)
  out = knit2pdf(
    input = rv$template_path,
    output = file.path(rv$rep_pdf, rv$output_filename), # lancement afi_Load
    # output = rv$output_filename, # lancement shiny
    # envir = db,
    clean = TRUE
  )
  
  
  
  ###### Job 10 : édition des résultats au format excel #####
  # gf_Tables2Xls(
  #   wd = rv$repGF,
  #   output_dir = rv$rep_sav,
  #   forest = rv$forest, last_cycle = rv$last_cycle,
  #   complete_progress = complete_progress,
  #   i18n = i18n
  #   )
  
  
  # ##### Job 7 : résultats SIG #####
  # wd = rv$repGF
  # output_dir = rv$rep_sav
  # forest = rv$forest
  # last_cycle = rv$last_cycle
  # 
  # # chargement du script
  # source(
  #   file.path(app_dir, "scripts/gf7_ShapesPlac_new.R"),
  #   encoding = 'UTF-8', echo = TRUE
  # )
  # 
  # # lancement
  # gf_ShapesPlac(wd = rv$repGF, output_dir = rv$rep_sav)
}




