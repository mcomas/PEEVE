library(tidyverse)
load('dades_n.RData')
glimpse(dades_n)
dplot = dades_n %>%
  pivot_longer(cols = c(consum_gas, consum_aigua, consum_electricitat))

ggplot(data=dplot) +
  geom_boxplot(aes(x = ringressos, y = values)) +


library(ggtern)
dplot = dades_n %>%
  filter(consum_electricitat>0, consum_aigua>0, consum_gas>0)
ggtern() +
  geom_mask() + 
  geom_point(data=dplot,
             aes(x=consum_electricitat, y=consum_aigua, z = consum_gas), col = 'blue') + 
  theme_classic()

Y = dplot %>%
  select(consum_electricitat, consum_aigua, consum_gas) %>%
  as.matrix()

X = dplot %>%
  select(ingressos) %>%
  as.matrix()

library(coda.base)
HY = coordinates(Y, 'clr')
colnames(HY) = paste0('clr.', colnames(Y))

summary(lm(HY~ringressos, data = dplot))
# A majors ingressos es veu com el consum relatiu d'aigua en relació a gas i electricitat baixa. 
# Segurament és perquè el consum d'aigua es manté constant.
summary(lm(consum_gas~ringressos, data = dplot))
summary(lm(consum_aigua~ringressos, data = dplot))
summary(lm(consum_electricitat~ringressos, data = dplot))
ggplot(data=dades_n) +
  geom_boxplot(aes(x = ringressos, y = consum_gas))

dpb = dplot %>%
  select(consum_electricitat, consum_aigua, consum_gas) %>%
  coordinates('pb')
plot_balance(basis(dpb))
ggplot(data = cbind(dplot, dpb)) +
  geom_point(aes(x = pb1, y = pb2, col = consum_desitjat), show.legend = FALSE) +
  labs(x = "log(gas / gm(aigua,electricitat))", y = "log(electricitat / aigua)") +
  theme_minimal()
