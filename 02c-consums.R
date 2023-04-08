library(tidyverse)
load('dades_n.RData')

library(ggtern)
dplot = dades_n %>%
  filter(consum_electricitat>0, consum_aigua>0, consum_gas>0)
ggtern() +
  geom_mask() + 
  geom_point(data=dplot,
             aes(x=consum_electricitat, y=consum_aigua, z = consum_gas), col = 'blue') + 
  theme_classic()

library(coda.base)
dpb = dplot %>%
  select(consum_electricitat, consum_aigua, consum_gas) %>%
  coordinates('pb')
plot_balance(basis(dpb))
ggplot(data = cbind(dplot, dpb)) +
  geom_point(aes(x = pb1, y = pb2, col = consum_desitjat), show.legend = FALSE) +
  labs(x = "log(gas / gm(aigua,electricitat))", y = "log(electricitat / aigua)") +
  theme_minimal()
