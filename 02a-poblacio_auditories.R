library(dplyr)
library(tidyr)
load('dades_n.RData')

library(readxl)
mun_com = read_excel("mpiscatalunya.xlsx", skip = 4, 
                     col_names = c('codi_mun', 'nom_mun', 'codi_com', 'nom_com', 'ss')) %>%
  mutate(
    codi_mun = sprintf("%06d", codi_mun)
  )

library(readr)
pob_mun = read_csv("Poblaci__de_Catalunya_per_municipi__rang_d_edat_i_sexe.csv", skip = 1,
                   col_types = c("icc______iii"), col_names = c('any', 'codi', 'nom', 't1','t2','t3')) %>%
  mutate(total = t1+t2+t3) %>%
  filter(any == '2020') %>%
  mutate(
    nom = if_else(nom == "Castell-Platja d'Aro", "Castell d'Aro", nom),
    nom = if_else(nom == "Lloret de Mar", "Lloret", nom)
  ) %>%
  inner_join(mun_com, by = c('codi' = 'codi_mun'))

# Origen auditoria
count(dades_n, cc)
filter(mun_com, ss == 1)

municipis_ss = count(dades_n, cc) %>%
  full_join(pob_mun, by = c('cc' = 'nom')) %>%
  filter(ss == 1) %>%
  replace_na(list(n=0)) %>%
  select(cc, n, codi_mun = codi, total)

com_ss0 = pob_mun %>%
  filter(ss == 0) %>%
  group_by(codi_com, nom_com) %>%
  summarise(total = sum(total))

comarques_ss = count(dades_n, cc) %>%
  inner_join(com_ss0, by = c('cc' = 'nom_com'))



save(comarques_ss, municipis_ss, mun_com, file = 'poblacio_auditories.RData')
