install.packages('data.table')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('plotly')
install.packages('shiny')
install.packages('shinyWidgets')
install.packages('shinydashboard')

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

dados <- fread('reclamacao.csv', encoding = 'UTF-8')
summary(dados)

#substituindo as colunas X.1, V1
reclamacao <- dados %>% 
  select(-X.1, -V1)

unique(reclamacao$regiao)

reclamacao <- reclamacao %>% 
  filter(regiao != 'N/D')

unique(reclamacao$regiao)
unique(reclamacao$Atendida)

#Removendo valores estranhos 
reclamacao$Atendida <- gsub(pattern = 'S|simiim', replacement = 'sim',
     reclamacao$Atendida)

reclamacao$Atendida <- gsub(pattern = 'N|nAoo|nao', replacement = 'não',
                            reclamacao$Atendida)

unique(reclamacao$Atendida)

#Removendo valor vazio
reclamacao <- reclamacao %>% 
  filter(Atendida != "")
unique(reclamacao$Atendida)


#Removendo sexo
reclamacao$SexoConsumidor <- gsub(pattern = 'N|NULL|N/I/I', replacement = 'N/I',
     x = reclamacao$SexoConsumidor)

unique(reclamacao$SexoConsumidor)

fwrite(reclamacao, 'dados_limpos.csv', row.names = F)

######################################################
##################   Graficos   ######################
######################################################

#Graficos1
grafico_atendida <- ggplot(reclamacao) +
  geom_bar(aes(Atendida), fill = c('red', 'green'),
           stat = 'count') +
  ylab('quantidade') +
  theme_bw()+
  ggtitle('Quantidade de chamadas')

grafico_atendida <- ggplotly(grafico_atendida)
grafico_atendida

#Graficos2
grafico_uf <- data.frame(table(reclamacao$UF)) %>% 
  rename(UF = Var1 , Qtd = Freq) %>% 
  ggplot(aes(x = reorder(UF, Qtd), y = Qtd,
             text = paste("UF:", UF, "<br>", "QTD:", Qtd)
             ))+
  geom_bar(fill = 'blue', stat = 'identity') +
  coord_flip() +
  xlab('UF') +
  theme_bw() +
  ggtitle('Quantidade de raclamação')

grafico_uf <- ggplotly(grafico_uf, tooltip = 'text')
grafico_uf

#Graficos3
#ano-mes-dia
grafico_data <- data.frame(table(as.Date(reclamacao$DataArquivamento))) %>% 
  rename(Data = Var1, Qtd = Freq) %>% 
  ggplot(aes(as.Date(Data), Qtd, 
             text = paste("Data:", as.Date(Data), "<br>", "N:", Qtd))) +
  geom_line(group = 1) +
  theme_bw() +
  ggtitle('Qtd Reclamações por mes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = '%b-%Y', breaks = '6 month') +
  xlab('Data')

grafico_data <- ggplotly(grafico_data, tooltip = 'text')
grafico_data

#exercicio
data.frame(table(as.Date(reclamacao$DataArquivamento))) %>%
  rename(Data = Var1, Qtd=Freq) %>%
  ggplot(aes(as.Date(Data), Qtd)) +
  geom_line(group = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  ggtitle('Quantidade de Reclamações por Ano-Mês') +
  scale_x_date(date_labels = '%d-%b-%y',breaks = '5 weeks')


#Graficos4
grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario, reclamacao$Atendida)) %>% 
  rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>% 
  ggplot() +
  geom_bar(aes(x = Ano, y = Qtd, fill = Atendida),
               stat = 'identity', position = position_dodge2())+
  theme_bw() +
  ggtitle('Quantidade de reclamações por ano')


grafico_atendida_ano <- ggplotly(grafico_atendida_ano)
grafico_atendida_ano


