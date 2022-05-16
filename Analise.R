library(readxl)
library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(lubridate)
library(ggThemeAssist)
library(formattable)
library(reactable)
library(esquisse)
library(reactablefmtr)

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
  pivot_longer(cols = c(ICMS, IPVA, ITCD, OUTROS,
                        `TOTAL GERAL DA RECEITA TRIBUTÁRIA`),
               names_to = "Tributos",
               values_to = "Montante") %>% 
  arrange(Período, desc(Montante)) %>% 
  mutate(Tributos = factor(Tributos,
                           levels = c("TOTAL GERAL DA RECEITA TRIBUTÁRIA",
                                      "ICMS",
                                      "IPVA",
                                      "ITCD",
                                      "OUTROS")))-> Impostos_Totais


StyleTheme <-  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title.x  = element_blank()) 

#Conteúdo 1


Impostos_Totais %>%
  ggplot(.,aes(x= Período, y = Montante, color= Tributos)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  StyleTheme +
  ggtitle("Arrecadação de Impostos do RN e Suas Evoluções (1997 - 2022)") + 
  scale_colour_manual(values = c("#32373b", "#62b851",
                                 "#009fb7", "#b20d30",
                                 "#f4b942"))

#Conteúdo 2
  

Impostos_Totais %>%
  filter(Período > "dez 2014",
         Tributos != "OUTROS") %>%
  mutate(Ano = year(Período)) %>%
  pivot_wider(names_from = Tributos, values_from = Montante) %>% 
  group_by(Ano) %>%
  summarise(ICMS = sum(ICMS),
            IPVA = sum(IPVA),
            ITCD= sum(ITCD),
            Total = sum(`TOTAL GERAL DA RECEITA TRIBUTÁRIA`)) %>% 
  pivot_longer(-c(Total, Ano),
               names_to = "Tributos",
               values_to = "Montante") %>% 
  mutate(prop = (Montante/Total)) %>%
  select(-c(Montante, Total)) %>% 
  pivot_wider(names_from = Tributos, values_from = prop)  %>% 
  reactable(defaultColDef = colDef(headerClass = "header", align = "center",
                                   headerStyle = list(
                                     background = "#7290ba",
                                     color  = "#f5f5f5")),
            columns = list(
              Ano = colDef(align = "left"),
              ICMS = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba")
              ),
              IPVA = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba")
              ),
              ITCD = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba")
              )
  ),
  bordered = T,
  striped = T
  ) %>% 
  google_font("Roboto")


#Conteúdo 3

RN_FISCAL %>% 
  filter(Período > "dez 2020") %>% 
  select(Período:`1.3 - TERCIÁRIO`, `TOTAL DA ARRECADAÇÃO DO ICMS`) -> SetoresI

SetoresI %>% 
  pivot_longer(cols = -c(Período, `TOTAL DA ARRECADAÇÃO DO ICMS`), 
               names_to = "Setores",
               values_to = "Montante") %>% 
  mutate(Setores = substring(Setores, first = 7)) %>% 
  ggplot(aes(Período, Montante, fill = Setores)) +
  geom_col(position = "stack") +
  StyleTheme +
  scale_fill_manual(values = c("#62b851", "#32373b",
                                 "#009fb7", "#b20d30",
                                 "#f4b942")) +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  ggtitle("Valor arrecadado do ICMS por setores")

#Conteúdo 4

