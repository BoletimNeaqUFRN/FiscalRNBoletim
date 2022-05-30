library(readxl)
library(tidyverse)
library(zoo)
library(ggplot2)
library(scales)
library(lubridate)
library(ggThemeAssist)
library(formattable)
library(reactable)
library(esquisse)
library(reactablefmtr)
library(ipeadatar)

X20220406_sigdef <- read_excel("20220406_sigdef.xls", 
                               sheet = "arrecadacao")

X20220406_sigdef %>% 
  select(-id_uf, -co_periodo) %>% 
  filter(ESTADO == "Rio Grande do Norte") %>% 
  mutate(Período = as.yearmon(paste(ANO, MÊS, sep= "-"))) %>% 
  select(ESTADO,Período, -(ANO:MÊS),
         `1.1 - PRIMÁRIO`:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`) %>% 
  filter(Período > "dez 1999") %>% 
  mutate(ITCD2 = ITCD,
         IPVA2 = IPVA) %>% 
  mutate(IPVA = case_when(Período =="mar 2018" ~ ITCD2,
                          T ~ IPVA),
         ITCD = case_when(Período =="mar 2018" ~ IPVA2,
                          T ~ ITCD)) %>% 
  select(-ITCD2, -IPVA2) -> RN_FISCAL

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


###### DEFLACIONADOR #####

ipeadata("PRECOS12_IPCA12")  %>% 
  filter(year(date) > 1999) %>% 
  mutate(Período = as.yearmon(date),
         value = value/value[1]) %>% 
  select(Período, value)  -> deflacionador



##### GRÁFICOS ####


StyleTheme <-  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title.x  = element_blank(),
        legend.box      = "horizontal",
        legend.position = "bottom",
        axis.text.x = element_text(size = 8, color = "Black"),
        axis.text.y = element_text(size = 8, color = "black")) 


destaque<- geom_rect(xmin=as.yearmon("jan 2020"),
                     xmax=as.yearmon("dez 2020"),
                     ymin=-Inf, 
                     ymax=Inf,
                     fill="lightgrey", 
                     alpha=0.03)

###### Evolução Receita Nominal 1 ####


Impostos_Totais %>%
  mutate(Montante=Montante*1e-6) %>% 
  ggplot(.,aes(x= Período, y = Montante, color= Tributos)) +
  destaque+
  geom_line(size = 1.1) +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  scale_x_yearmon(format="%b%Y", 
                  breaks = seq(from = min(Impostos_Totais$Período),
                               to = max(Impostos_Totais$Período),
                               by = 0.5))+
  StyleTheme +
  labs(title = "Arrecadação de Impostos do RN e Suas Evoluções (1997 - 2022)",
       subtitle = "(Valores Nominais - Milhões de reais)",
       caption = "Observatório Conjuntura Econômica do RN \n NEAQ-DEPEC/UFRN \n Fonte: Confaz (2022)", position = c("left", "top"))+
  scale_colour_manual(values = c("#32373b", "#62b851",
                                 "#009fb7", "#b20d30",
                                 "#f4b942"))+
  theme(axis.text.x = element_text(angle = 90))

##### Participação dos Impostos na Arrecadação do Estado  #####
  

Impostos_Totais %>%
  filter(Período > "dez 2014") %>%
  mutate(Ano = year(Período)) %>%
  pivot_wider(names_from = Tributos, values_from = Montante) %>% 
  group_by(Ano) %>%
  summarise(ICMS = sum(ICMS),
            IPVA = sum(IPVA),
            ITCD= sum(ITCD),
            OUTROS = sum(OUTROS),
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
                                 fill_color = "#7290ba",
                                 max_value=1)
              ),
              IPVA = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)
              ),
              ITCD = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)
              ),
              OUTROS = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)
              )),
            bordered = T,
            striped = T
  ) %>% 
  google_font("Roboto")

##### Arrecadação por Setor  #####

RN_FISCAL %>% 
  filter(Período > "dez 2018") %>% 
  select(Período:`1.3 - TERCIÁRIO`, `2.1 - ENERGIA ELÉTRICA`, `2.2 - PETRÓLEO, COMBUSTÍVEIS E LUBRIFICANTES`)  -> Setores

Setores %>% 
  mutate(Período = as.yearqtr(Período, format = "%y Q%q")) %>%
  group_by(Período) %>%
  summarise(across(everything(), sum))%>% 
  pivot_longer(cols = -Período, 
               names_to = "Setores",
               values_to = "Montante") %>% 
  mutate(Setores = substring(Setores, first = 7),
         Montante = Montante/1000000)-> Setores1

Setores1%>%
  ggplot(aes(Período, Montante, fill = Setores)) +
  geom_col(position = "stack") +
  StyleTheme +
  scale_fill_manual(values = c("#62b851", "#32373b",
                               "#009fb7", "#b20d30",
                               "#f4b942"))  +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  scale_x_yearqtr(format = "%Y\nQ%q", 
                  breaks = seq(from = min(Setores1$Período),
                               to = max(Setores1$Período),
                               by = 0.2)) +
  theme(axis.text.x = element_text(angle = 0))
ggtitle("Valor arrecadado do ICMS por setores")



Setores1%>%
  group_by(Período) %>% 
  mutate(Total=sum(Montante),
         prop=round((Montante/Total), 2)) %>% 
  separate(col=Período,into = c("Ano","Trimestre"), sep =" ")%>% 
  mutate(Trimestre=str_replace_all(Trimestre, "Q", "T")) %>% 
  select(Ano, Trimestre, Setores, prop)%>% 
  pivot_wider(names_from = Setores, values_from = prop)   %>% 
  reactable(defaultColDef = colDef(headerClass = "header", align = "center",
                                   headerStyle = list(
                                     background = "#7290ba",
                                     color  = "#f5f5f5")),
            columns = list(
              Ano = colDef(align = "left", maxWidth = 90),
              Trimestre=colDef(align = "left", maxWidth = 90),
              PRIMÁRIO = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)),                
              SECUNDÁRIO = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)),
              TERCIÁRIO = colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)),
              `ENERGIA ELÉTRICA`= colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)),
              `PETRÓLEO, COMBUSTÍVEIS E LUBRIFICANTES`= colDef(
                cell = data_bars(., 
                                 number_fmt = formattable::percent,
                                 text_position = "outside-base",
                                 force_outside = c(0,0.4),
                                 fill_color = "#7290ba",
                                 max_value=1)),
              bordered = T,
              striped = T ,
              pagination = F
           )) %>% 
  google_font("Roboto")





#Conteúdo 4

pervar <- function(x) { #função de variação percentual
  (x - lag(x))/lag(x)
}

RN_FISCAL %>% 
  select(ESTADO,
         Período,
         `TOTAL DA ARRECADAÇÃO DO ICMS`,
         IPVA,
         ITCD,
         `TOTAL GERAL DA RECEITA TRIBUTÁRIA`) %>%
  rename(ICMS = `TOTAL DA ARRECADAÇÃO DO ICMS`) %>% 
  filter(Período > "nov 2017") %>% 
  mutate(across(ICMS:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`, pervar)) -> variacao_percentual_arrecadacao


RN_FISCAL %>%
  select(ESTADO,
         Período,
         `TOTAL DA ARRECADAÇÃO DO ICMS`,
         IPVA,
         ITCD,
         `TOTAL GERAL DA RECEITA TRIBUTÁRIA`) %>%
  rename(ICMS = `TOTAL DA ARRECADAÇÃO DO ICMS`) %>%
  filter(Período > "dez 2018") %>%
  mutate(Período = as.yearqtr(Período, format = "%y Q%q")) %>%
  group_by(Período) %>%
  summarise(across(everything()[-1], sum)) %>%
  mutate(across(ICMS:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`, ~ .x/.x[1])) %>%
  pivot_longer(cols = ICMS:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`,
               names_to = "Tributos", values_to = "Montante") %>%
  mutate(Tributos = factor(Tributos,
                           levels = c("TOTAL GERAL DA RECEITA TRIBUTÁRIA",
                                      "ICMS",
                                      "IPVA",
                                      "ITCD",
                                      "OUTROS"))) -> num_indice_arrecadacao


ggplot(num_indice_arrecadacao, aes(x = Período,y = Montante, fill = Tributos, label = percent(Montante))) +
  geom_bar(stat = "identity", position = "dodge") +
  StyleTheme +
  theme(axis.text.x = element_text(angle = 22.5)) +
  scale_x_yearqtr(format = "%YQ%q") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  scale_fill_manual(values = c("#32373b",
                               "#62b851",
                               "#009fb7", "#b20d30",
                               "#f4b942")) +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 2021.92, y = 1.5,
           label = "24.13%", angle = 90,
           hjust = 0.4, vjust = 0.2,
           fontface = "bold", size = 3.7) +
  annotate("text", x = 2022, y = 1.5,
           label = "15.74%", angle = 90,
           hjust = 0.4, vjust = -0.4,
           fontface = "bold",size = 3.7) +
  annotate("text", x = 2022, y = 1.5,
           label = "48.41%", angle = 90,
           vjust = 1, hjust = 0.4,
           fontface = "bold",size = 3.7) +
  annotate("text", x = 2022.06, y = 1.5,
           label = "23.79%", angle = 90,
           vjust = 1, hjust = -.07,
           fontface = "bold",size = 3.7) +
  labs(y = "Variação Percentual",
       title = "Variação Percentual Nominal dos Tributos do RN (2019 - 2022)",
       caption = "Observatório Conjuntura Econômica do RN \n NEAQ-DEPEC/UFRN \n Fonte: Confaz (2022)", position = c("left", "top"))

  
# num_indice_arrecadacao[(nrow(num_indice_arrecadacao)-3):nrow(num_indice_arrecadacao),3] %>%
#   mutate_all(scales::percent) %>% 
#   as.vector()  -> teste      

# Comentando pois podemos automatizar os gráficos utilizando certos macetes

##### RECEITA REAL #####

Impostos_Totais %>% 
  left_join(., deflacionador) %>% 
  mutate(`Valor Real` = Montante/value) %>% 
  ggplot(.,aes(x= Período, y = `Valor Real`, color= Tributos)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  StyleTheme +
  labs(title = "Arrecadação de Impostos do RN e Suas Evoluções (1997 - 2022)",
       subtitle = "(Valores Reais)",
       caption = "Observatório Conjuntura Econômica do RN \n NEAQ-DEPEC/UFRN \n Fonte: Confaz (2022)", position = c("left", "top"))+
  scale_colour_manual(values = c("#32373b", "#62b851",
                                 "#009fb7", "#b20d30",
                                 "#f4b942"))
  
  #####  Variação Real 2019-2022 #####
  
  RN_FISCAL %>%
    select(ESTADO,
           Período,
           `TOTAL DA ARRECADAÇÃO DO ICMS`,
           IPVA,
           ITCD,
           `TOTAL GERAL DA RECEITA TRIBUTÁRIA`) %>%
    rename(ICMS = `TOTAL DA ARRECADAÇÃO DO ICMS`) %>%
    left_join(., deflacionador) %>% 
    filter(Período > "dez 2018") %>%
    mutate(value = value/value[1]) %>% 
    mutate(ICMS = ICMS/value,
           IPVA = IPVA/value,
           ITCD = ITCD/value,
           `TOTAL GERAL DA RECEITA TRIBUTÁRIA` = `TOTAL GERAL DA RECEITA TRIBUTÁRIA`/value) %>% 
    mutate(Período = as.yearqtr(Período, format = "%y Q%q")) %>%
    group_by(Período) %>%
    summarise(across(everything()[-c(1,6)], sum)) %>%
    mutate(across(ICMS:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`, ~ .x/.x[1])) %>%
    pivot_longer(cols = ICMS:`TOTAL GERAL DA RECEITA TRIBUTÁRIA`,
                 names_to = "Tributos", values_to = "Montante") %>%
    mutate(Tributos = factor(Tributos,
                             levels = c("TOTAL GERAL DA RECEITA TRIBUTÁRIA",
                                        "ICMS",
                                        "IPVA",
                                        "ITCD",
                                        "OUTROS"))) %>% 
    ggplot(., aes(x = Período,y = Montante, fill = Tributos, label = percent(Montante))) +
    geom_bar(stat = "identity", position = "dodge") +
    StyleTheme +
    theme(axis.text.x = element_text(angle = 22.5)) +
    scale_x_yearqtr(format = "%YQ%q") +
    theme(axis.text.x=element_text(angle=45, hjust=1))+
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
    scale_fill_manual(values = c("#32373b",
                                 "#62b851",
                                 "#009fb7", "#b20d30",
                                 "#f4b942")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Variação Percentual",
         title = "Variação Percentual Real dos Tributos do RN (2019 - 2022)",
         caption = "Observatório Conjuntura Econômica do RN \n NEAQ-DEPEC/UFRN \n Fonte: Confaz (2022)", position = c("left", "top"))
  
  
  
  


