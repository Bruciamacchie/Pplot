res <- Arbres |>
  group_by(Cycle) |>
  summarise(Gha = sum(Gha)/180,
            Vha = sum(Vha)/180,
            VcHa = sum(VcHa)/180,
            VpHa = sum(VpHa)/180,
            GainHa = sum(GainHa/180))

res1 = res
res2 = res |>
  mutate(Cycle = Cycle +1)
Resultat = rbind(res1, res2)


load("Tables/gfTablesElaboreesPlac.RData")
gfPlaEssCat |>
  group_by(Cycle, EssReg, Cat) |>
  summarise(VcHa = sum(VcHa)/180) |>
  ggplot(aes(x=Cat, y=VcHa, fill=EssReg)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ Cycle) +
  theme_bw()


t1 <- Arbres1 |>
  group_by(Cycle, Statut) |>
  summarise(Gha = sum(Gha)/180,
            Vha = sum(Vha)/180,
            VcHa = sum(VcHa)/180,
            VpHa = sum(VpHa)/180,
            GainHa = sum(GainHa/180))

t2 <- Arbres1 |>
  dplyr::select(NumForet, Strate, NumPlac, NumArbre, Cycle,Statut,Diam,Gha,Vha) |>
  arrange(NumForet, Strate, NumPlac, NumArbre, Cycle, Gha) |>
  group_by(NumForet, Strate, NumPlac, NumArbre) |>
  mutate(AccGper = Gha - lag(Gha))

t3 <- t2 |>
  filter(Statut == "Coupe") |>
  group_by(NumPlac) |>
  summarise(Vha = sum(Vha))

sum(t3$Vha)/180



