library(tidyverse)

load('dades_n.RData')
load('poblacio_auditories.RData')
load('prima_vulnerabilitat.RData')
taula = pvulnera %>%
  left_join(distinct(mun_com, codi_com, nom_com), by = c('nom_comarca' ='nom_com')) %>%
  select(codi_com, prima_vul)

com_prima = comarques_ss %>% 
  left_join(taula, by = 'codi_com') %>%
  select(cc, n, total, prima_vul)
mun_prima = municipis_ss %>% 
  left_join(mun_com, by = 'codi_mun') %>% 
  left_join(taula, by = 'codi_com') %>%
  select(cc, n, total, prima_vul)

primes = bind_rows(com_prima, mun_prima) %>%
  mutate(
    auditories = 1000 * n / total, 
    ratio = auditories / prima_vul) %>%
  arrange(desc(ratio))

ggplot(data=primes) +
  geom_point(aes(x = auditories, y = prima_vul)) 
cor(primes[,4:5])
