library(dplyr)
library(readxl)
dades = read_excel('data/22_06_02 DRIVE CC.xlsx')
glimpse(dades)

rn_vrs = c(cc = "CC",
           time = 'Marca de temps',
           id_auditoria = "CODI D'AUDITORIA",
           grau_urg = "Grau d'urgència determinat pel Centre Coordinador.",
           dcontact = "Data de contacte amb la família",
           electro = "Subministrament d'electrodomèstics",
           ringressos = "Rang d'ingressos")

library(lubridate)
library(tidyr)
dades_n = dades %>%
  select(rn_vrs) %>% 
  mutate(
    electro = as.integer(if_else(electro == 'Sí, No', NA, electro == 'Sí')),
    dcontact = as.Date(as.numeric(dcontact), origin = "1899-12-30"),    # Perdem valor a id_auditoria = 117155701020
    ringressos = factor(ringressos, levels = c("Menys de 12.449€", "de 12.450€ a 19.999€", "de 20.000€ a 35.199€",
                                               "de 35.200€ a 59.999€", "Més de 60.000€"))
  ) 

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

pob_com_menor10000$total %>% sum()

pob_all = bind_rows(
  pob_mun %>% select(nom, pob = total),
  pob_com_menor10000 %>% select(nom = nom_com, pob = total))

taula_ingressos %>%
  left_join(pob_all, by = c('cc' = 'nom')) %>%
  mutate(per1000 = 1000 * `Menys de 12.449€` / pob)

save(dades_n, file = 'dades_n.RData')
