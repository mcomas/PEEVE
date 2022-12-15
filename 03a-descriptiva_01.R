library(tidyverse)

load('dades_n.RData')
load('poblacio_auditories.RData')

pob_all = bind_rows(comarques_ss, municipis_ss)

#########################
# Tant per mil auditories sobre població base
index_auditories = pob_all %>%
  group_by(cc) %>%
  summarise(n = sum(n), total = first(total)) %>%
  transmute(cc, auditories = 1000 * n / total) %>%
  mutate(cc = fct_reorder(cc, auditories)) %>%
  arrange(desc(auditories))

p_auditories = ggplot(data = index_auditories) + 
  geom_hline(yintercept = 2, col = 'red') +
  geom_bar(aes(x = cc, y = auditories), stat = 'identity', alpha = 0.75, width = 0.85) +
  labs(y = 'Auditories cada 1000 habitants', x = '', title = 'Index d\'auditories',
       subtitle = 'Totes les auditories') +
  coord_flip() +
  theme_minimal()
p_auditories

index_auditories_social = pob_all %>%
  filter(is.na(eix) | eix == 'social') %>%
  group_by(cc) %>%
  summarise(n = sum(n), total = first(total)) %>%
  transmute(cc, auditories = 1000 * n / total) %>%
  mutate(cc = fct_reorder(cc, auditories)) %>%
  arrange(desc(auditories))

p_auditories_social = ggplot(data = index_auditories_social) + 
  geom_hline(yintercept = 2, col = 'red') +
  geom_bar(aes(x = cc, y = auditories), stat = 'identity', alpha = 0.75, width = 0.85) +
  labs(y = 'Auditories cada 1000 habitants', x = '', title = 'Index d\'auditories',
       subtitle = 'Només eix social') +
  coord_flip() +
  theme_minimal()
p_auditories_social

save(index_auditories, index_auditories_social, file = '03a-descriptiva_01.RData')

# library(patchwork)
# p_auditories + p_auditories_social
# 
# ggsave(p_auditories, p_auditories_social,
#        file = 'auditories.pdf', width = 6.5, height = 4.6)

# 
# taula_ingressos = dades_n %>% 
#   count(cc, ringressos) %>%
#   pivot_wider(names_from = ringressos, values_from = n, values_fill = 0L)
# taula_ingressos
# 
# taula_ingressos %>%
#   left_join(pob_all, by = c('cc')) %>%
#   mutate(ing_baixos_rel_pob = 1000 * `Menys de 12.449€` / total) %>%
#   arrange(desc(ing_baixos_rel_pob))
# 
# # Relació categòria llar vs consum deitjat
# count(dades_n, categoria_llar, consum_desitjat) %>% 
#   pivot_wider(names_from = consum_desitjat, values_from = n, values_fill = 0)
# 
