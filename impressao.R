library(tidyverse)
library(dplyr)

data <- read.csv(file.choose(), sep = ",")
data

#create new dataframe with structure
#USUARIO | MES | MONO | COLOR | MACRO
#df <- data.frame(c("usuario", "mes", "mono", "color", "unidade"))
df <- data.frame(usuario=character(), mes=double(), mono=double(), color=double(), unidade=character())

year <-"2018"
month <- "11"

year_month = c()

for(i in 1:52){
  month_year <- as.numeric(paste(as.character(year),as.character(month), sep = ''))
  if(month == "12") {
    month <- "01"
    year <- as.numeric(year)+1
  }else {
    month <- as.numeric(month)+1
    if(month < 10) month <- paste("0", as.character(month), sep = '')
    month <- as.character(month)
  }
  year_month <- append(year_month, month_year)
}

sum_mono <- function(user, month){
  
  row <- data%>%
    filter(USUÁRIO == user)
  
  col_name = paste("MONO", substr(as.character(month),5,6), substr(as.character(month),1,4), sep="_")
  
  return(as.numeric(row[col_name]))
  
}

#c <- sum_mono(data[1,1], 202211)

sum_color <- function(user, month){
  
  row <- data%>%
    filter(USUÁRIO == user)
  
  col_name = paste("COLOR", substr(as.character(month),5,6), substr(as.character(month),1,4), sep="_")
  
  return(as.numeric(row[col_name]))
  
}

get_unidade <- function(user, month){
  
  row <- data%>%
    filter(USUÁRIO == user)
  
  col_name = paste("MACRO", substr(as.character(month),5,6), substr(as.character(month),1,4), sep="_")
  
  if (col_name %in% names(data)){
    return(as.character(row[col_name]))  
  }
  
  return("")
  
}

#percorre a lista de meses
for(month in year_month){
  #repete cada mês para cada usuário
  for(i in 1:nrow(data)){
    #print(month)
    new_row = list(usuario=data[i,1], mes=month, mono=sum_mono(data[i,1], month), color=sum_color(data[i,1], month), unidade=get_unidade(data[i,1], month)) 
    #print(new_row)
    df <- rbind(df, new_row)
    print(paste(as.character(month),i))
  }
}

head(df)

#SEPARETE MONTH AND YEAR
df <- df %>%
  mutate(ano = substr(as.character(mes),1,4)) 

df <- df %>%
  mutate(mes = substr(as.character(mes),5,6))

#SAVE INTO CSV FORMAT
#USUARIO | MES | MONO | COLOR | MACRO | ANO
write.csv(df, "/home/ruminiki.schmoeller/impressao.csv", row.names=T) 

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

df <- NULL
df <- read.csv(file.choose(), sep = ",")
df %>% summarize(total = sum(mono))
#COMPARAÇÃO TOTAL MONO POR ANO
df %>%
  group_by(ano) %>%
  summarize(mono = sum(mono, na.rm = TRUE)) %>%
  ggplot(aes(x=ano, y=mono, fill=mono))+
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Ano") + ylab("Total páginas monocromáticas") + 
  scale_fill_viridis() +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(2018, 2023, by = 1)) +
  geom_text(aes(label = format(round(mono), big.mark = ".")), 
            colour = "black", size = 3.5, vjust = -0.51)

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
  geom_histogram(binwidth = 100, alpha=0.6, color = "#2C3E50", fill = "#22A884FF")+
  geom_vline(aes(xintercept = quantile(mono,0.50)), linetype="dashed", color = "red") +
  #geom_vline(aes(xintercept = quantile(mono,0.75)), linetype="dashed", color = "orange") + 
  geom_vline(aes(xintercept = quantile(mono,0.79)), linetype="dashed", color = "#440154FF") + 
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
  group_by(usuario, ano) %>%
  summarize(mono = sum(mono)) %>%
  filter(mono > 1000) %>%
  arrange(desc(mono))

#---------------------------------------
df %>%
  ggplot(aes(x = ano, y = mono)) +
  geom_point(color = "black") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm",
              formula = y ~ splines::bs(x),
              se = FALSE, size = 2) +
  geom_text_repel(aes(label = usuario), # pacote ggrepel
                  size = 2,
                  color = "black",
                  max.overlaps = 100) +
  labs(y = "Violações de Trânsito em NY (logs)",
       x = "Índice de Corrupção dos Países") +
  scale_color_manual("Label:",
                     values = "gold") +
  facet_wrap(~ano) +
  theme_bw()











#####################################################33
glimpse(df)

df <- sample_n(df, 5000)

df$ano <- factor(df$ano)
df$unidade <- factor(df$unidade)
df$mes <- factor(df$mes)

##################################################################################
#              OBSERVANDO OS DADOS CARREGADOS DA BASE planosaude                 #
##################################################################################
#Estatísticas univariadas
summary(df)

#Categorias da variável 'plano'
levels(factor(df$ano))

#Tabela de frequências absolutas da variável 'plano'
table(df$ano)

##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################
cols <- c(3, 7:9)
chart.Correlation((dummies[cols]), histogram = TRUE)

#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################

dummies <- dummy_columns(.data = df,
                                 select_columns = "ano",
                                 remove_selected_columns = T,
                                 remove_most_frequent_dummy = T)

dummies <- dummies %>%
  select(-X, -mes, -usuario, -color, -unidade) 

chart.Correlation((dummies[1:6]), histogram = TRUE)

#Visualizando a base de dados dummizada
dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

##################################################################################
#                       ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA                   #
##################################################################################
#Modelagem com todas as variáveis
modelo <- lm(mono ~ ., dummies)

#Parâmetros do modelo_planosaude
summary(modelo)
vif(modelo)

head(dummies)
dummies$ypred <- modelo$fitted.values

chart.Correlation((dummies[c(1,7)]), histogram = TRUE)

ggplotly(
  ggplot(dummies, aes(x = mono, y = ypred)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    theme_classic()
)

##################################################################################
#                               PROCEDIMENTO STEPWISE                            #
##################################################################################

step_modelo <- step(modelo, k = 3.841459)

summary(step_modelo)

##################################################################################
#            TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE        #
##################################################################################

#Teste de Shapiro-Francia
sf.test(step_modelo$residuals) #função 'sf.test' do pacote 'nortest'

#Plotando os resíduos do modelo step_planosaude 
df %>%
  mutate(residuos = step_modelo$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
df %>%
  mutate(residuos = step_modelo$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_planosaude$residuals),
                            sd = sd(step_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE) - forma não-paramétrica para estimar a
#função densidade de probabilidade de uma variável aleatória
dummies %>%
  ggplot() +
  geom_density(aes(x = step_modelo$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

##################################################################################
#                        DIAGNÓSTICO DE HETEROCEDASTICIDADE                      #
##################################################################################

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(step_modelo)
#função 'ols_test_breusch_pagan' do pacote 'olsrr'
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#Adicionando fitted values e resíduos do modelo 'step_planosaude'
#no dataset 'planosaude_dummies'
dummies$fitted_step <- step_modelo$fitted.values
dummies$residuos_step <- step_modelo$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox

dummies <- dummies %>% filter(mono > 0)

lambda_BC <- powerTransform(dummies$mono)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
dummies$bcmono <- (((dummies$mono ^ lambda_BC$lambda) - 1) / 
                                    lambda_BC$lambda)

#Visualizando a nova variável na base de dados



dummies %>%
  select(mono, bcmono, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

#Estimando um novo modelo múltiplo com dummies
modelo_bc <- lm(formula = bcmono ~ . -mono -fitted_step 
                           -residuos_step, 
                           data = dummies)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_bc <- step(modelo_bc, k = 3.841459)

summary(step_bc)

#Verificando a normalidade dos resíduos do modelo step_bc_planosaude
#Teste de Shapiro-Francia
sf.test(step_bc$residuals) #função 'sf.test' do pacote 'nortest'

#Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teórica
dummies %>%
  mutate(residuos = step_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_planosaude$residuals),
                            sd = sd(step_bc_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE)
dummies %>%
  ggplot() +
  geom_density(aes(x = step_bc$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_planosaude)

#Adicionando fitted values e resíduos do modelo 'step_bc_planosaude'
#no dataset 'planosaude_dummies'
dummies$fitted_step_novo <- step_bc$fitted.values
dummies$residuos_step_novo <- step_bc$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_bc_planosaude'
dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()

#################################### FIM ########################################



