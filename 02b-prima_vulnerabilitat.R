library(pdftools)

fulles = pdf_text("docs/21_09_2018_11_42_43_2_Metodologia_pobresa_energetica.pdf")

library(tidyverse)
pvulnera = tibble(
  rows = c(str_split(fulles[28], "\n")[[1]][-c(1:5, 39:40)],
           str_split(fulles[29], '\n')[[1]][-c(1:3, 12:14)]) %>%
    str_replace_all(",", ".") %>%
  str_replace_all(regex("  +"), ";")) %>%
  separate(rows, c('nom_comarca', 'pes_pob', 'pes_pob_adj', 'prima_vul'), sep = ';') %>%
  mutate(across(-nom_comarca, as.numeric)) %>%
  pull(prima_vul)

save(pvulnera, file = 'prima_vulnerabilitat.RData')