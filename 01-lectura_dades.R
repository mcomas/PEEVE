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
                                 "text", "text", "text", "text"))


dades2 = read_excel("data/22_06_02 DRIVE CC.xlsx", 
                    sheet = "3.DOC", col_types = c("text", 
                                                   "numeric", "text", "numeric", 
                                                   "text", "text", "text", "text", "numeric", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "numeric", "text", "text", 
                                                   "text", "text", "text", "text"))

# Per poder unir les dues fulles de l'Excel es descarten les observacions amb codi d'auditoria repetit.

dades1 = dades1 %>%
  group_by(`CODI D'AUDITORIA`) %>%
  filter(n() == 1) %>%
  ungroup()
dades2 = dades2 %>%
  group_by(Columna1) %>%
  filter(n() == 1) %>%
  ungroup()

dades = full_join(dades1, dades2, by = c("CODI D'AUDITORIA" = 'Columna1'))

glimpse(dades)
# Carreguem auditories i polim dades rebudes.
names(dades)
rn_vrs = c(cc = "CC.x",
           time = 'Marca de temps.x',
           id_auditoria = "CODI D'AUDITORIA",
           grau_urg = "Grau d'urgència determinat pel Centre Coordinador.",
           dcontact = "Data de contacte amb la família.x",
           electro = "Subministrament d'electrodomèstics.x",
           ingressos = "Quin va ser l'import total d'ingressos bruts rebuts pel conjunt de la unitat familiar durant l'any 2019? (8)",
           ringressos = "Rang d'ingressos",
           membres = "Total composició de la unitat familiar (Dones i Homes)",
           subministre_enderrariment = "En els últims 12 mesos, ha tingut enderrariments en el pagament de subministraments degut a dificultats econòmiques? (20)",
           subministre_tall = "En els últims 12 mesos, ha tingut talls dels subministraments bàsics? (21)",
           subministre_deute = "Té deutes pendents en relació als subministraments bàsics, en cas afirmatiu indiqui l'import? (23, 24 )",
           categoria_llar = "En quina categoria energètica de la llar considera que està? (45)",
           consum_desitjat = "Fa tot el consum energètic que desitjaria? (46)",
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
    consum_aigua = parse_number(consum_aigua)
  ) 

save(dades_n, file = 'dades_n.RData')
