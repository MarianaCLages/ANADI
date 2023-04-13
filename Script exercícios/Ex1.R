#Trabalho Prático 1

#1.
#a)
# Importar dados do ficheiro DADOS1.csv
getwd()
setwd("C:/Users/maria/Desktop/ISEP/3ºano/2ºsemestre/ANADI/Trabalho Prático 1")
dados1 <- read.csv("DADOS1.csv", header = TRUE, skip = 2)

# Converter segundos em POSIXct
dados1$Tempo <- as.POSIXct(dados1$s, origin = "1970-01-01", tz = "GMT")

# Verificar se a nova coluna "Tempo" foi adicionada corretamente
head(dados1) #ou head(dados1$Tempo)


#b)
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o dia 4 de agosto de 2013
dados_filtrados_tempo <- dados1 %>%
  filter(Tempo >= as.POSIXct("2013-08-04 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2013-08-04 23:59:59", tz = "GMT"))

# Filtrar os dados para as temperaturas do motor de cada bomba
#K.1 -> Medições da temperatura do motor na bomba 1
#K.3 -> Medições da temperatura do motor na bomba 2
#K.5 -> Medições da temperatura do motor na bomba 3
dados_filtrados_temperatura <- dados_filtrados_tempo %>%
  select(Tempo, K.1, K.3, K.5)

# Package necessário para criar o gráfico
library(ggplot2)
ggplot(dados_filtrados_temperatura, aes(x = Tempo)) +
  geom_line(aes(y = K.1, color = "Bomba 1")) +
  geom_line(aes(y = K.3, color = "Bomba 2")) +
  geom_line(aes(y = K.5, color = "Bomba 3")) +
  labs(title = "Temperatura do Motor",
       x = "Tempo",
       y = "Temperatura",
       color = "Bomba") +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_bw()


#c)
# Criar o boxplot com as três bombas
boxplot(dados_filtrados_temperatura[,2:4], # selecionar as colunas com as temperaturas
        names = c("Bomba 1", "Bomba 2", "Bomba 3"), # nomes das bombas
        xlab = "Bomba", ylab = "Temperatura", # rótulos dos eixos
        main = "Temperatura do motor de cada bomba")


#d)
#i.
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o mês de março de 2014
dados_filtrados_tempo2 <- dados1 %>%
  filter(Tempo >= as.POSIXct("2014-03-01 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2014-03-31 23:59:59", tz = "GMT"))

# Filtrar os dados para as medições de oil rate de cada bomba
# bbl.d.2 -> Medições do oil rate da bomba 1
# bbl.d.5 -> Medições do oil rate da bomba 2
dados_filtrados_oil_rate <- dados_filtrados_tempo2 %>%
  select(Tempo, bbl.d.2, bbl.d.5)

colnames(dados_filtrados_oil_rate)[2:3]  <- c("Oil rate (Bomba 1)","Oil rate (Bomba 2)")

# Calcular a média diária para cada bomba (quantidade de barris produzida)
medias_diarias <- dados_filtrados_oil_rate %>%
  group_by(as.Date(Tempo)) %>%
  summarise(medias_diarias_bomba1 = mean(`Oil rate (Bomba 1)`),
            medias_diarias_bomba2 = mean(`Oil rate (Bomba 2)`))

colnames(medias_diarias)[1]  <- "Dia"

# Packages necessários para unir colunas de tabela (tidyr) e criar o gráfico de barras (ggplot2)
library(tidyr)
library(ggplot2)

# Unir as colunas "medias_diarias_bomba1" e "medias_diarias_bomba2"
medias_diarias <- medias_diarias %>% 
  gather(key = "bomba", value = "medias_diarias_bombas", -Dia)

# Criar o gráfico de barras
ggplot(medias_diarias, aes(x = Dia, y = medias_diarias_bombas, fill = bomba)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(title = "Barris de petróleo produzidos diariamente no mês de março de 2014",
       x = "Dia",
       y = "Nº de barris de petróleo produzidos (média diária)",
       fill = "Bomba") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d") +
  scale_fill_manual(values = c("coral1", "cadetblue"),
                    labels = c("Bomba 1", "Bomba 2")) +
  theme_bw()


#ii.
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o período especificado
dados_filtrados_tempo3 <- dados1 %>%
  filter(Tempo >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2014-05-31 23:59:59", tz = "GMT"))

# Calcular a média mensal de extração de barris de petróleo da bomba 1
medias_mensais_bomba1 <- dados_filtrados_tempo3 %>%
  mutate(Mes = format(Tempo, "%m-%Y")) %>%
  group_by(Mes) %>%
  summarise(Media = mean(bbl.d.2))

# Encontrar o mês em que a extração foi a maior e armazenar na variável "mes_maior_valor"
mes_maior_valor <- medias_mensais_bomba1 %>%
  slice(which.max(medias_mensais_bomba1$Media)) %>%
  pull(Mes) ; mes_maior_valor

# O mês em que a bomba 1 extraiu mais barris de petróleo foi em agosto de 2013 (08-2013).


#iii.
# Package necessário para filtrar dados
library(dplyr)

# Filtrar os dados para o período especificado
dados_filtrados_tempo4 <- dados1 %>%
  filter(Tempo >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") &
           Tempo <= as.POSIXct("2014-05-31 23:59:59", tz = "GMT")) %>%
  select(Tempo, bbl.d.2, bbl.d.5)

colnames(dados_filtrados_tempo4)[2:3] <- c("Bomba 1", "Bomba 2")

# Convertendo os números de dias em datas
data_inicial <- as.Date("2013-06-01")
amostra_dias <- c(78, 362, 66, 156, 277, 41, 339, 169, 272, 268)
amostra_datas <- data_inicial + amostra_dias - 1 ; amostra_datas

# Datas correspondentes da amostra aleatória (amostra_datas)
# "2013-08-17" "2014-05-28" "2013-08-05" "2013-11-03" "2014-03-04" "2013-07-11" "2014-05-05"
# "2013-11-16" "2014-02-27" "2014-02-23"

producao_amostra <- dados_filtrados_tempo4 %>%
  filter(as.Date(Tempo) %in% amostra_datas) %>%
  select(Tempo, `Bomba 1`, `Bomba 2`)

boxplot(producao_amostra[, 2:3], 
        col = c("coral", "lightblue"), 
        names = c("Bomba 1", "Bomba 2"), 
        xlab = "Bomba", 
        ylab = "Produção diária",
        main = "Produção diária por bomba")


#iv.
# H0: média da produção diária de petróleo da bomba 1 <= média da produção diária de petróleo da bomba 2
# H1: média da produção diária de petróleo da bomba 1 > média da produção diária de petróleo da bomba 2

# Teste t para 2 amostras independentes
t.test(producao_amostra$`Bomba 1`, producao_amostra$`Bomba 2`, alternative = "greater", conf.level = 0.95)

# Como p < 2.2e-16 < alfa (0.05) => rejeita-se H0.
# Há evidências estatísticas para afirmar que a média da produção diária de petróleo da bomba 1 é maior do que a da bomba 2 no período de 1-6-2013 e 31-5-2014, com 5% de significância.


#v.
# Calcular as médias amostrais
media_bomba1 <- mean(producao_amostra$`Bomba 1`)
media_bomba2 <- mean(producao_amostra$`Bomba 2`)

# Comparando as médias amostrais
diferenca <- media_bomba1 - media_bomba2
if (diferenca > 0) {
  cat("A média da produção diária de petróleo da bomba 1 é maior do que a da bomba 2.")
} else if (diferenca < 0) {
  cat("A média da produção diária de petróleo da bomba 2 é maior do que a da bomba 1.")
} else {
  cat("As médias são iguais.")
}
