library(tidyverse)
library(dplyr)

data = NULL
data <- read.csv(file.choose(), sep = ";")
data

summary(data)


#ANÁLISES
install.packages("hrbrthemes")
install.packages("factoextra")
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(cluster)
library(factoextra)

boxplot(data$total_impresso)

#COMPARAÇÃO TOTAL MONO POR ANO
total <- data %>% summarize(total = sum(total_impresso))
total <- as.double(total)

data %>%
  group_by(unidade) %>%
  summarize(total_i = sum(total_impresso, na.rm = TRUE)) %>%
  ggplot(aes(x=unidade, y=total_i/(total), fill=total_i))+
  geom_col(position = "dodge") + 
  xlab("Unidade") + ylab("Percentual impresso") + 
  scale_fill_viridis() +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(total_i/total)), 
            colour = "black", size = 3.5, vjust = -0.51)

data %>% summarize(total = sum(total_impresso))


data %>%
  count(unidade, status) %>%
  ggplot(aes(x=unidade, y=n, fill=status))+
  geom_bar(position="identity", stat="identity") +
  xlab("Unidade") + ylab("Total impressoras") + 
  scale_fill_viridis(discrete = T) +
  theme(legend.position="top", legend.title = element_blank()) + 
  geom_text(aes(label = n), 
            colour = "black", size = 3.5, vjust = -0.51)

data %>% count(status)
data$localizacao <- as.factor(data$localizacao)

data %>%
  ggplot(aes(x=total_impresso))+
  geom_histogram(binwidth = 15000) +
  geom_vline(aes(xintercept = quantile(total_impresso,0.25)), linetype="dashed", color = "red") +
  geom_vline(aes(xintercept = quantile(total_impresso,0.50)), linetype="dashed", color = "orange") +
  geom_vline(aes(xintercept = quantile(total_impresso,0.75)), linetype="dashed", color = "orange") +
  scale_x_continuous(breaks = seq(0, 150000, by = 15000)) +
  facet_wrap(~ unidade)

data %>% filter(unidade == "VILA-A")
summary(data_pti$total_impresso)

aux <- data %>% filter(status != "Devolvida") %>% filter(status != "Disponível")  %>% filter(status != "Empréstimo") %>% filter(unidade == "VILA A")
aux %>% count(total_impresso >= quantile(aux$total_impresso,0.25))







df%>%
  group_by(ano, mes) %>%
  summarise(mono = sum(mono)) %>%
  mutate(ano_mes = paste(ano, mes)) %>%
  ggplot(aes(x=reorder(ano, -mes), y=mono, fill=mono)) +
  geom_bar(stat = "identity", position = "fill") + 
  xlab("Ano") + ylab("Total páginas monocromáticas") + 
  theme(legend.position="none") +
  geom_bar(stat = "identity") +
  scale_fill_viridis()

df %>%
  group_by(ano, mes) %>%
  summarize(mono = sum(mono, na.rm = TRUE)) %>%
  ggplot(aes(x=mes, y=mono, fill=mono))+
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  xlab("Ano") + ylab("Total páginas monocromáticas") + 
  theme(legend.position="none") +
  facet_wrap(~ ano)


#HISTOGRAMA CONSUMO USUÁRIO MONO
#calculate binwidth para geral
df_by_user = df %>% 
  group_by(usuario, ano) %>% 
  summarise(mono = sum(mono))

data_points = n_distinct(df$usuario)
max = max(df_by_user$mono)
min = min(df_by_user$mono)
bin_width = (max - min) / sqrt(data_points) 

#, y = after_stat(count/sum(count))

df_by_user %>%
  ggplot(aes(x=mono))+
  geom_histogram(binwidth = 200, color="#e9ecef", alpha=0.8) +
  scale_x_continuous(breaks = seq(0, 15000, by = 500)) 


###########################################
#calc binwidth para ano
df_by_user_2022_color = df%>%
  filter(ano == 2022) %>%
  group_by(usuario, ano) %>%
  summarise(color = sum(color)) %>%
  arrange(desc(color))

df_by_user_2022_mono = df %>%
  filter(ano == 2022) %>%
  group_by(usuario, ano) %>%
  summarise(mono = sum(mono)) %>%
  arrange(desc(mono)) %>%
  mutate(name = fct_reorder(usuario, desc(mono)))

df_by_user_2022_mono %>%
  ggplot(aes(x=name, y=mono))+
  #geom_histogram(binwidth = 100, alpha=0.6, color = "#2C3E50", fill = "#9B59B6") +
  geom_col() +
  geom_vline(aes(xintercept = quantile(mono,0.50)), linetype="dashed", color = "red") +
  geom_vline(aes(xintercept = quantile(mono,0.95)), linetype="dashed", color = "orange") +
  scale_x_discrete(limits = df_by_user_2022_mono$usuario) +
  xlab("Total páginas monocromáticas") + ylab("Total usuários") + 
  theme(legend.position="none")

df_by_user_2022_mono %>%
  ggplot(aes(x=mono))+
  geom_histogram(binwidth = 100, alpha=0.6, color = "#2C3E50", fill = "#9B59B6")+
  geom_vline(aes(xintercept = quantile(mono,0.50)), linetype="dashed", color = "red") +
  #geom_vline(aes(xintercept = quantile(mono,0.75)), linetype="dashed", color = "orange") + 
  geom_vline(aes(xintercept = quantile(mono,0.79)), linetype="dashed", color = "orange") + 
  scale_x_continuous(breaks = seq(100, 6000, by = 100)) + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

quantile(df_by_user_2022_mono$mono,0.75)

nrow(df_by_user_2022_mono[df_by_user_2022_mono$mono < 101,])/nrow(df_by_user_2022_mono)
nrow(df_by_user_2022_mono[df_by_user_2022_mono$mono > 100 & df_by_user_2022_mono$mono < 200,])/nrow(df_by_user_2022_mono)
nrow(df_by_user_2022_mono[df_by_user_2022_mono$mono > 200 & df_by_user_2022_mono$mono < 300,])/nrow(df_by_user_2022_mono)
nrow(df_by_user_2022_mono[df_by_user_2022_mono$mono > 300 & df_by_user_2022_mono$mono < 400,])/nrow(df_by_user_2022_mono)
nrow(df_by_user_2022_mono[df_by_user_2022_mono$mono > 400 & df_by_user_2022_mono$mono < 500,])/nrow(df_by_user_2022_mono)
nrow(df_by_user_2022_mono[df_by_user_2022_mono$mono > 500,])/nrow(df_by_user_2022_mono)

(sum(df_by_user_2022_mono[df_by_user_2022_mono$mono > 500,]$mono) / sum(df_by_user_2022_mono$mono))*100

nrow(df_by_user_2022_color[df_by_user_2022_color$color > 0 & df_by_user_2022_color$color < 51,])/nrow(df_by_user_2022_color)
nrow(df_by_user_2022_color[df_by_user_2022_color$color > 31 & df_by_user_2022_color$color < 40,])/nrow(df_by_user_2022_color)
nrow(df_by_user_2022_color[df_by_user_2022_color$color > 41 & df_by_user_2022_color$color < 50,])/nrow(df_by_user_2022_color)
nrow(df_by_user_2022_color[df_by_user_2022_color$color > 51 & df_by_user_2022_color$color < 60,])/nrow(df_by_user_2022_color)
nrow(df_by_user_2022_color[df_by_user_2022_color$color > 61,])/nrow(df_by_user_2022_color)

sum(df_by_user_2022_color[df_by_user_2022_color$color > 100,]$color) / sum(df_by_user_2022_color$color)

nrow(df_by_user_2022)
sum(df_by_user_2022$mono)

#CONSUMO POR MÊS (density)

newdf <- df%>%
  filter(ano %in% c(2020,2022,2023), mes %in% c(1,2))

newdf %>%
  group_by(ano, mes) %>%
  summarise(mono = sum(mono)) %>%
  mutate(ano_mes = paste(ano,mes)) %>%
  ggplot(aes(x=ano_mes, y=mono, fill=ano)) +
  geom_col() +
  xlab("Ano/mês") + ylab("Total páginas monocromáticas") + 
  theme(legend.position="none") +
  geom_text(aes(label = format(round(mono), big.mark = ".")), 
            colour = "black", size = 3.5, vjust = -0.51) +
  scale_fill_viridis()

newdf2 <- df%>%
  filter(ano %in% c(2019,2022))%>%
  group_by(ano, mes) %>%
  summarize(mono = sum(mono)) %>%
  mutate(median = median(mono)) %>%
  mutate(mean = mean(mono))

total_2019 <- df %>% filter(ano == 2019) %>% group_by(ano) %>% summarise(mono = sum(mono))
total_2022 <- df %>% filter(ano == 2022) %>% group_by(ano) %>% summarise(mono = sum(mono))

total_2022$mono / total_2019$mono * 100



##########################################
#clustering
library(factoextra)

mono.scaled <- scale(df_by_user_2022_mono$mono)

# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(mono.scaled, 6, nstart = 40)

df_cluster3=cbind(df_by_user_2022_mono$mono, cluster=km.res$cluster)
head(df_cluster3)

# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
fviz_cluster(km.res, df_cluster3, alpha = 0.2, shape = 19, geom = c("point"))

fviz_cluster(km.res, data=df_cluster3,
             #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             
             ggtheme=theme_minimal()
)

#CORRELAÇÃO CONSUMO MÊS
#não é linear, aumenta o mês aumenta o consumo. Tem que ser não supervisionado - classificação?
df %>%
  group_by(mes) %>%
  summarise(mono = sum(mono)) %>%
  ggplot(aes(mes, mono)) + geom_point()






#USUARIOS COM CONSUMO ACIMA DA MÉDIA
newdf <- df %>%
  summarize(mono = sum(mono)) %>%
  arrange(ano)

df_color <- df %>%
  filter(color > 50) %>%
  group_by(usuario) %>%
  summarize(color = sum(color)) %>%
  arrange(desc(color))



df %>%
  ggplot(aes(x = ano, y = mono))+
  geom_boxplot()


summary(df)





