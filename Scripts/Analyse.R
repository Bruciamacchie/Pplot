
sum(Arbres$VpHaRisq)/sum(Arbres$VpHa)

load("Tables/gfTablesElaboreesPlac.RData")
gfPlaEssCat |>
  group_by(Cycle, EssReg, Cat) |>
  summarise(VcHa = sum(VcHa)/180) |>
  ggplot(aes(x=Cat, y=VcHa, fill=EssReg)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ Cycle) +
  theme_bw()








