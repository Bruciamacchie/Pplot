############## Librairies ##############
librarian::shelf(tidyverse,sf,readxl,stars,terra,gstat,automap,xtable,patchwork,ggthemes)

# -------- Répertoire = to source file location
setwd("C:/Users/amand/Documents/FIF/PAFE_amenagement/krigeage/krigeageR")

########### IMPORT #############
parc <- st_read("ParcFor.gpkg") %>%
  st_make_valid()#corrige des erreurs de topologie

########### MODIF #############
perim <- parc  |> #créer un ojet avec table attributaire
  group_by()  |>
  summarise() |>
  st_sf()

plac <- st_read("PP_GPSL_corrige3.gpkg")

mnh <- rast("mnh_bon_2022.tif")  |>
  crop(st_buffer(perim, dist=100))  |> #permet de se focaliser sur notre foret à partir d'un grand raster
  project("EPSG:2154") #après crop on peut utiliser mask pour fit aux limites
names(mnh) <- "mnh"


############## KRIGEAGE #############
pas = 25
grid = st_as_sfc(st_bbox(perim))
grid = st_as_stars(grid, dx = pas, dy = pas)
mne = st_warp(src = mnh, grid, method = "average", use_gdal = TRUE)#meme résolution;origine et projection que le mnh
mne = mne[perim]#ce qui est dans le permimetre
mne = st_normalize(mne)
names(mne) = "mnh"
#grid = tableau avec les cases pour passer des donnees ponctuelles à continues
#maintenant il faut remplir les cases avec la suite

# plot(mne)


# --------- Uniquement fonction de la distance ------------
vgm = autofitVariogram(Gha ~ 1, as(plac, "Spatial"))#vient de automap vieux truc qui
#comprend pas les objets sf mais fixe à notre place ce qui est utilisé dans la fonction
#en dessous
#~1 veut dire que ça prend en compte que la distance
#as(plac,"Spatial") pour convertir le sf
#plus besoin de cette ligne car MAJ
g = gstat(formula = Gha ~ 1, model = vgm$var_model, data = plac)
z = predict(g, mne)

b = seq(0, 40, 2)
plot(z, breaks = b, col = hcl.colors(length(b)-1, "Spectral"), reset = FALSE)
plot(st_geometry(parc), pch = 3, add = TRUE, border='blue')
contour(z, breaks = b, add = TRUE)

# --------- Avec MNH ------------
#car si chablis juste distance ca le prend pas en compte par ex
plac <- plac %>%
  mutate(mnh10 = extract(mnh, vect(st_buffer(plac, dist=10)), fun=mean, na.rm=T)$mnh,
         mnh15 = extract(mnh, vect(st_buffer(plac, dist=15)), fun=mean, na.rm=T)$mnh,
         mnh20 = extract(mnh, vect(st_buffer(plac, dist=20)), fun=mean, na.rm=T)$mnh,
         mnh25 = extract(mnh, vect(st_buffer(plac, dist=25)), fun=mean, na.rm=T)$mnh)

vgmh = autofitVariogram(Gha ~ mnh15, as(plac, "Spatial"))
g = gstat(formula = Gha ~ mnh15, model = vgmh$var_model, data = plac)
z = predict(g, mne)

# b = seq(0, floor(max(plac$Gha/4+0.5)*4), 4)
b = c(0,5,15,seq(20,40,5))
plot(z, breaks = b, col = hcl.colors(length(b)-1, "Spectral"), reset = FALSE)
plot(st_geometry(parc), pch = 3, add = TRUE, border='blue')
contour(z, breaks = b, add = TRUE, col='grey')
