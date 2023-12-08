library(hrbrthemes)
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(cluster)
library(factoextra)

df <- NULL
df <- read.csv(file.choose(), sep = ",")

by_user <- df %>% 
  group_by(usuario, ano) %>%
  summarize(total = sum(mono)) %>%
  arrange(desc(total)) 

#-------------------------------

by_month <- df %>%
  select(mes, ano, mono) %>%
  group_by(mes, ano) %>%
  summarise(mono = sum(mono)) %>%
  mutate(mes_ano = as.factor(paste(ano, mes, sep=""))) %>% 
  arrange(ano, mes) 

ts=ts(by_month$mono,start=c(2018,11), end=c(2023,3), frequency = 12)
plot(ts)

by_month %>%
  filter(ano == 2019) %>%
  ggplot(aes(x=mes_ano, y=mono, group=1)) +
  geom_point() +
  geom_line()


ggplotly(
by_month %>%
  ggplot(aes(x=mes_ano, y=mono, group=1)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_discrete(limits=by_month$mes_ano) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))

#----------------------

tsoutliers(by_month$mono)

by_month %>%
  mutate(mono_suave = tsclean(mono),
         media_movel = SMA(mono, 2)) %>%
  ggplot() +
  geom_line(aes(x=mes_ano, y=mono, color="Série Original", group=1)) +
  geom_line(aes(x=mes_ano, y=mono_suave, color="Série Suavizada", group=1)) +
  geom_line(aes(x=mes_ano, y=media_movel, color="Média Móvel", group=1)) +
  labs(color = "Legenda:",
       x = "Data",
       y = "Comportamento da Covid-19") +
  scale_color_viridis_d() +
  scale_x_discrete(limits=by_month$mes_ano) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")


## Cálculo da média móvel não centralizada

ggplotly(
  by_month %>%
    ggplot(aes(x=mes_ano, y=mono, group=1)) +
    geom_point() +
    geom_line() +
    scale_color_viridis_d() +
    scale_x_discrete(limits=by_month$mes_ano) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))


ggplotly(
  covid %>%
    mutate(Data = as.Date(Data),
           media_movel_nao_centralizada = SMA(por_dia, 14)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = por_dia, color = "Por Dia")) +
    geom_line(aes(x = Data, y = media_movel_nao_centralizada,
                  color = "Média Móvel Não Centralizada"), size = 1) +
    labs(color = "Legenda:",
         x = "Data",
         y = "Comportamento da Covid-19") +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))


#=================

group_by(mes, ano) %>%
  summarise(mono = sum(mono)) %>%
  mutate(mes_ano = as.factor(paste(ano, mes, sep=""))) %>% 
  arrange(ano, mes) 

df <- df%>%mutate(mes_ano = as.factor(paste(ano, mes, sep="")))
data <- df %>%
  filter(usuario %in% c("claudio.monteiro","livia.morales")) %>%
  group_by(usuario, mes, ano) %>%
  mutate(mes_ano = as.factor(paste(ano, mes, sep="")))

data %>%
  ggplot() +
  geom_line(aes(x=mes_ano, y=mono, color="Série Original", group=1)) +
  #geom_line(aes(x=mes_ano, y=mono_suave, color="Série Suavizada", group=1)) +
  #geom_line(aes(x=mes_ano, y=media_movel, color="Média Móvel", group=1)) +
  labs(color = "Legenda:",
       x = "Data",
       y = "Comportamento da Covid-19") +
  scale_color_viridis_d() +
  scale_x_discrete(limits=df$mes_ano) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom") +
  facet_wrap(data$ano)


#---------------------
df_sum <-
  df %>%
    select(mes_ano, mes, ano, mono) %>%
    group_by(mes_ano, mes, ano) %>%
    summarise(mono = sum(mono))

df_sum %>%
  ggplot() +
  geom_line(aes(x=mes_ano, y=mono, color="Série Original", group=1)) +
  #geom_line(aes(x=mes_ano, y=mono_suave, color="Série Suavizada", group=1)) +
  #geom_line(aes(x=mes_ano, y=media_movel, color="Média Móvel", group=1)) +
  labs(color = "Legenda:",
       x = "Data",
       y = "Comportamento da Covid-19") +
  scale_color_viridis_d() +
  scale_x_discrete(limits=df$mes_ano) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")
    
df_sum <- df_sum %>% filter(ano >= 2022) %>% arrange(ano, mes)

base=ts(df_sum$mono,start = c(2022,1),
        end = c(2023,2), frequency = 12)

autoplot(base)

modeloholt =holt(base,h=12, level=0.95)

# valores previstos

modeloholt

# modelo gerado
modeloholt$model

# valores estimados

modeloholt$fitted

# visualização dos dados e das previ?es com intervalos de confiança

autoplot(modeloholt)

