library(dplyr)
library(readxl)
library(stringr)
dades1 = read_excel("data/22_06_02 DRIVE CC.xlsx", 
                   col_types = c("text", "numeric", "text", 
                                 "numeric", "text", "text", "text", 
                                 "text", "numeric", "text", "text", 
                                 "numeric", "text", "text", "numeric", 
                                 "text", "text", "numeric", "text", 
                                 "numeric", "text", "numeric", "text", 
                                 "numeric", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "numeric", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text")) %>%
  mutate(`Data de contacte amb la família` = as.Date(as.numeric(`Data de contacte amb la família`), origin = "1899-12-30"))


dades2 = read_excel("data/22_06_02 DRIVE CC.xlsx", 
                    sheet = "3.DOC", col_types = c("text", 
                                                   "numeric", "text", "numeric", 
                                                   "text", "text", "text", "text", "numeric", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "numeric", "text", "text", 
                                                   "text", "text", "text", "text"))%>%
  mutate(`Data de contacte amb la família` = as.Date(as.numeric(`Data de contacte amb la família`), origin = "1899-12-30"))


keep1 = dades1 %>%
  filter(
    FALSE |
      (`CODI D'AUDITORIA` == "117034000720" & `Data de contacte amb la família` == "2020-12-11") |
      (`CODI D'AUDITORIA` == "117095001021" & `Marca de temps` < 44299.54) |
      (`CODI D'AUDITORIA` == "117095002821" & `Data de contacte amb la família` == "2021-06-22") | 
      (`CODI D'AUDITORIA` == "117117501920" & `Marca de temps` < 44020.62) | #
      (`CODI D'AUDITORIA` == "117117506820" & `Data de contacte amb la família` == "2020-09-24") |
      (`CODI D'AUDITORIA` == "117117507520" & `Marca de temps` < 44018.48) |
      (`CODI D'AUDITORIA` == "117141100720" & `Marca de temps` < 44072) |
      (`CODI D'AUDITORIA` == "117147905320" & `Marca de temps` < 44180) |
      (`CODI D'AUDITORIA` == "117155701420" & CC == 'Salt') 
  ) %>%
  select(`CODI D'AUDITORIA`, `Data de contacte amb la família`, `Marca de temps`, CC)
keep2 = dades2 %>%
  filter(
    FALSE |
      (Columna1 == "117034000820" & `Marca de temps` > 44245) |
      (Columna1 == "117117501920" & `Marca de temps` < 44021) |
      (Columna1 == "117117506820" & `Marca de temps` > 44112) | 
      (Columna1 == "117117507520" & `Marca de temps` < 44018.5) | #
      (Columna1 == "117117512020" & `Marca de temps` > 44172.5) |
      (Columna1 == "117141101720" & `Marca de temps` < 44181.6) |
      (Columna1 == "117147900320" & `Marca de temps` < 44034) |
      (Columna1 == "117147900720" & `Marca de temps` < 44030) |
      (Columna1 == "117147901520" & `Marca de temps` < 44030) |
      (Columna1 == "117147905120" & `Marca de temps` < 44033) |
      (Columna1 == "417184800221" & `Indicar el preu de l'energia (93)` == "140") 
  ) %>%
  select(Columna1, `Marca de temps`, `Indicar el preu de l'energia (93)`)

  
# Per poder unir les dues fulles de l'Excel es descarten les observacions amb codi d'auditoria repetit.
dades1_final = bind_rows(
  dades1 %>%
    group_by(`CODI D'AUDITORIA`) %>%
    filter(n() == 1) %>%
    ungroup(),
  dades1 %>%
    semi_join(keep1))

dades2_final = bind_rows(
  dades2 %>%
    group_by(Columna1) %>%
    filter(n() == 1) %>%
    ungroup(),
  dades1 %>%
    semi_join(keep1))

dades = full_join(dades1_final, dades2_final, by = c("CODI D'AUDITORIA" = 'Columna1'))

glimpse(dades)
# Carreguem auditories i polim dades rebudes.
names(dades)
rn_vrs = c(cc = "CC.x",
           time = 'Marca de temps.x',
           id_auditoria = "CODI D'AUDITORIA",
           grau_urg = "Grau d'urgència determinat pel Centre Coordinador..x",
           dcontact = "Data de contacte amb la família.x",
           electro = "Subministrament d'electrodomèstics.x",
           ingressos = "Quin va ser l'import total d'ingressos bruts rebuts pel conjunt de la unitat familiar durant l'any 2019? (8).x",
           ringressos = "Rang d'ingressos.x",
           bo.social = "Indicar si disposa de bo social (94, 95)",
           membres = "Total composició de la unitat familiar (Dones i Homes).x",
           subministre_enderrariment = "En els últims 12 mesos, ha tingut enderrariments en el pagament de subministraments degut a dificultats econòmiques? (20).x",
           subministre_tall = "En els últims 12 mesos, ha tingut talls dels subministraments bàsics? (21).x",
           subministre_deute = "Té deutes pendents en relació als subministraments bàsics, en cas afirmatiu indiqui l'import? (23, 24 ).x",
           categoria_llar = "En quina categoria energètica de la llar considera que està? (45).x",
           consum_desitjat = "Fa tot el consum energètic que desitjaria? (46).x",
           contracte_electricitat = "Indicar tipus de contracte (87, 97). [Contracte per electricitat]",
           potencia_electricitait = "Indicar potència contractada (88).",
           consum_electricitat = "Indicar el consum d'electricitat en els últims 12 mesos (92)",
           contracte_gas = "Indicar tipus de contracte (87, 97). [Contracte per Gas natural ]",
           consum_gas = "Indicar el consum de gas natural en els últims 12 mesos (100)",
           consum_aigua = "Indicar el consum d'aigua en els últims 12 mesos (103)")

library(lubridate)
library(tidyr)
library(readr)
dades_n = dades %>%
  select(rn_vrs) %>% 
  mutate(
    eix = NA_character_,
    eix = if_else(str_starts(id_auditoria, '1'), 'social', eix),
    eix = if_else(str_starts(id_auditoria, '3'), 'salut', eix),
    eix = if_else(str_starts(id_auditoria, '4'), 'ciutadania', eix),
    electro = as.integer(if_else(electro == 'Sí, No', NA, electro == 'Sí')),
    dcontact = as.Date(as.numeric(dcontact), origin = "1899-12-30"),    # Perdem valor a id_auditoria = 117155701020
    ringressos = factor(ringressos, levels = c("Menys de 12.449€", "de 12.450€ a 19.999€", "de 20.000€ a 35.199€",
                                               "de 35.200€ a 59.999€", "Més de 60.000€")),
    consum_electricitat = parse_number(consum_electricitat),
    consum_aigua_str = consum_aigua,
    consum_aigua_pou = as.integer(str_detect(consum_aigua_str, 'pou')),
    consum_aigua = parse_number(consum_aigua)
  ) 

save(dades_n, file = 'dades_n.RData')
