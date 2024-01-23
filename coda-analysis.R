library(tidyverse)
library(ggtern)
library(patchwork)
load('dades_n.RData')
library(coda.base)
dat = dades_n %>%
  mutate(
    consum_gas = if_else(consum_gas > 100000000/2, NA_real_, consum_gas),
    consum_electricitat = if_else(consum_electricitat > 100000, NA_real_, consum_electricitat)) %>%
  select(consum_electricitat, consum_gas, consum_aigua,
         electro, ingressos, membres)

dplot = dat %>%
  pivot_longer(starts_with('consum_'))
p_raw = ggplot(data = dplot) +
  geom_boxplot(aes(y = value)) +
  facet_wrap(~name, scales = 'free') 

p_log = ggplot(data = dplot) +
  geom_boxplot(aes(y = value)) +
  facet_wrap(~name, scales = 'free') +
  scale_y_continuous(trans = 'log', breaks = 2^(1:50))

p_raw / p_log


dat = filter(dat, consum_electricitat > 0, consum_gas > 0, consum_aigua > 0)
summary(dat)

summary(lm(ingressos~coord(consum_electricitat, consum_gas, consum_aigua, basis='alr'), data=dat))
summary(lm(ingressos~coord(consum_electricitat, consum_aigua, consum_gas, basis='alr'), data=dat))
# (Intercept)                                             6127.0     2345.0   2.613  0.00971 **
# consum_electricitat / {consum_gas, consum_aigua}        814.9      687.4   1.185  0.23733   
# consum_gas / {consum_electricitat, consum_aigua}        737.5      499.9   1.475  0.14179
# consum_aigua / {consum_gas, consum_electricitat}      -1552.4      758.3  -2.047  0.04201 *

# menys ingressos menys consum d'aigua (segurament menys ingressos impliqui menys membres)

summary(lm(membres~coord(consum_aigua, consum_electricitat, consum_gas, basis='alr'), data=dat))
summary(lm(membres~coord(consum_electricitat, consum_gas, consum_aigua, basis='alr'), data=dat))
# (Intercept)                                             6127.0     2345.0   2.613  0.00971 **
# consum_electricitat / {consum_gas, consum_aigua}       -0.5350     0.1281  -4.176 4.54e-05 *** 
# consum_gas / {consum_electricitat, consum_aigua}      -0.02389    0.09316  -0.256    0.798 
# consum_aigua / {consum_gas, consum_electricitat}        0.5589     0.1413   3.955 0.000108 ***

## El consum d'aigua es dispara quan més membres

## Intento ajustar la ratio entre consum_aigu i les demés
B = sbp_basis(cbind(c(1,-1,-1)))
summary(lm(cbind((consum_aigua*consum_electricitat*consum_gas)^(1/3), 
                 coord(consum_aigua, consum_electricitat, consum_gas, basis = B))~ingressos+membres, data = dat))
## Ajustat per ingressos i membre, més membres més consum d'aigua/altres, però quan més ingressos menys aigua/altres.

variation_array(select(dat, consum_aigua, consum_electricitat, consum_gas), include_means = TRUE)
# total variance
sum(diag(cov(coordinates(select(dat, consum_aigua, consum_electricitat, consum_gas)))))

ggtern(data = select(dat, consum_aigua, consum_electricitat, consum_gas)) +
  geom_point(aes(x=consum_aigua, y=consum_electricitat, z=consum_gas))


# remotes::install_github('mcomas/coda.plot')
library(coda.plot)
# Forma
clr_biplot(select(dat, consum_aigua, consum_electricitat, consum_gas))
# Covariance
clr_biplot(select(dat, consum_aigua, consum_electricitat, consum_gas), alpha = 0)

