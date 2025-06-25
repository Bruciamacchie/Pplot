librarian::shelf(tidyverse, sf, readxl)

rep = "~/pCloudSync/EnCours/AutoEntreprise/Vibraye/SIG/DataBrut/cadastre"
Insees <- c(72032, 72156, 72158, 72333, 72373)
fichiers = paste(rep, Insees, sep="-","parcelles.json")

cadCom <- st_sf(st_sfc(), crs = 2154)

for (i in 1:length(Insees)) {
  cad <- st_read(fichiers[i]) |> 
    st_transform(2154)
  cadCom = rbind(cadCom, cad)
}

cadCom <- cadCom |> 
  rename(INSEE = commune,
         Section = section,
         Numéro = numero,
         Surface = contenance) |> 
  mutate(Numéro = as.numeric(Numéro)) |> 
  dplyr::select(INSEE, Section, Numéro, Surface)


foncier <- read_excel("~/pCloudSync/EnCours/AutoEntreprise/Vibraye/Cadastre/Foncier.xlsx") |> 
  dplyr::select(Section,Numéro,INSEE) |> 
  left_join(cadCom, by = join_by(INSEE, Section, Numéro)) |> 
  st_sf()


plot(st_geometry(foncier))
st_write(foncier, "foncier.gpkg", append=TRUE)
