library(readxl)
library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(lubridate)
library(formattable)

X20220406_sigdef <- read_excel("20220406_sigdef.xls", 
                               sheet = "arrecadacao")

X20220406_sigdef %>% 
  select(-id_uf, -co_periodo) %>% 
  filter(ESTADO == "Rio Grande do Norte") %>% 
  mutate(Período = as.yearmon(paste(ANO, MÊS, sep= "-"))) %>% 
  select(ESTADO,Período, -(ANO:MÊS),
         `1.1 - PRIMÁRIO`:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`) %>% 
  filter(Período > "dez 1999") -> RN_FISCAL

RN_FISCAL %>% 
  select(ESTADO,
         Período,
         `TOTAL DA ARRECADAÇÃO DO ICMS`,
         ITCD,
         IPVA,
         OUTROS,
         `TOTAL GERAL DA RECEITA TRIBUTÁRIA`) %>%
  rename(ICMS = `TOTAL DA ARRECADAÇÃO DO ICMS`) %>%
  pivot_longer(cols = c(ICMS, IPVA, ITCD, OUTROS),
               names_to = "Tributos",
               values_to = "Montante") -> Impostos_Totais


StyleTheme <-  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title.x  = element_blank())


Impostos_Totais %>%
  ggplot(.,aes(x= Período, y = Montante, color= Tributos)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  StyleTheme +
  ggtitle("Arrecadação de Impostos do RN e Suas Evoluções (1997 - 2022)")



Impostos_Totais %>% 
  filter(Período > "dez 2021") 
  

Impostos_Totais %>%
  filter(Período > "dez 2014") %>%
  mutate(Ano = year(Período)) %>%
  group_by(Ano, Tributos) %>%
  summarise(Montante = sum(Montante),
            Total = sum(`TOTAL GERAL DA RECEITA TRIBUTÁRIA`)) %>%
  mutate(prop = round(Montante/Total, digits = 2)) %>%
  filter(Tributos != "OUTROS") %>%
  ggplot(., aes(x = Ano, y = Montante, label = prop)) +
  StyleTheme +
  geom_col(position = "stack",aes(fill = Tributos)) +
  geom_text(hjust = "center")



  




