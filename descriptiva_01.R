library(dplyr)
load('dades_n.RData')

taula_ingressos = dades_n %>% 
  count(cc, ringressos) %>%
  pivot_wider(names_from = ringressos, values_from = n, values_fill = 0L)
taula_ingressos

library(readr)
mun_com = read_csv2("mpiscatalunya.csv", skip = 4, col_types = c('cccc_'),
                    col_names = c('codi_mun', 'nom_mun', 'codi_com', 'nom_com'))
pob_mun = read_csv("Poblaci__de_Catalunya_per_municipi__rang_d_edat_i_sexe.csv", skip = 1,
                   col_types = c("icc______iii"), col_names = c('any', 'codi', 'nom', 't1','t2','t3')) %>%
  mutate(total = t1+t2+t3) %>%
  filter(any == '2020') %>%
  mutate(
    nom = if_else(nom == "Castell-Platja d'Aro", "Castell d'Aro", nom),
    nom = if_else(nom == "Lloret de Mar", "Lloret", nom)
  ) %>%
  inner_join(mun_com, by = c('codi' = 'codi_mun'))

pob_mun %>%
  filter(total >= 10000, nom_com == 'Baix Empordà')

pob_com_menor10000 = pob_mun %>%
  filter(total < 10000) %>%
  group_by(codi_com, nom_com) %>%
  summarise(total = sum(total)) %>%
  ungroup()

pob_all = bind_rows(
  pob_mun %>% select(nom, pob = total),
  pob_com_menor10000 %>% select(nom = nom_com, pob = total))

taula_ingressos %>%
  left_join(pob_all, by = c('cc' = 'nom')) %>%
  mutate(ing_baixos_rel_pob = 1000 * `Menys de 12.449€` / pob) %>%
  arrange(desc(ing_baixos_rel_pob))

# Relació categòria llar vs consum deitjat
count(dades_n, categoria_llar, consum_desitjat) %>% 
  pivot_wider(names_from = consum_desitjat, values_from = n, values_fill = 0)

