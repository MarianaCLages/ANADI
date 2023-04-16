#Exercício 2

# Primeiro efetua-se a definicao do caminho em que se encontra o nosso data set
setwd("C:/Users/MiguelJordão(1201487/Desktop/ANADI/anadi23") 

#Ler os dados do CSV.file
dadosCSV <- read.csv("Dados utilizados/DADOS2.csv")

# Selecionar as colunas correspondentes às precisões de cada algoritmo
precisoes <- dadosCSV[, 3:8]

# Calcular a matriz de correlações
correlacoes <- cor(precisoes)

#Imprimir os valores da matriz
print(correlacoes)

#          SVM        DT        KN        RF        ML        GB
#SVM 1.0000000 0.2619970 0.6374486 0.4659169 0.7111270 0.8629016
#DT  0.2619970 1.0000000 0.4339395 0.8802559 0.6247459 0.2127059
#KN  0.6374486 0.4339395 1.0000000 0.4834416 0.8539377 0.7502013
#RF  0.4659169 0.8802559 0.4834416 1.0000000 0.5719541 0.3240401
#ML  0.7111270 0.6247459 0.8539377 0.5719541 1.0000000 0.7211135
#GB  0.8629016 0.2127059 0.7502013 0.3240401 0.7211135 1.0000000

# 0.8802559 - maior relação entre DT e RF
# 0.2127059 - menor relação entre DT e GB
# restantes valores variam entre 0.2619970 e 0.8629016

#Utilizamos a função "cor" que é mais correta para estudar a comparação entre pares de variáveis
#Pois poderia ter sido utilizado o "rcorr" (que se baseava em spearman e pearson, mas ao analisarmos a população de dados deduzimos que não é possível utilizar nenhum dos dois)

# Uma maneira de visualizar a matriz numa forma gráfica é utilizar o corrplot para construir um diagrama a partir dos dados

library(corrplot)

corrplot(correlacoes, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

#b)

# Para podermos efetuar afirmações sobre uma determinada população de dados, recorremos
# a testes paramétricos. Para tal, temos de efetuar os seguintes passos:

# Primeiro, formulamos a nossa hipótese de testes:
#H0: Existem diferenças significativas entre a precisão dos diferentes algoritmos
#H1: Não existem diferenças significativas entre a precisão dos diferentes algoritmos

# Em segundo, a variável dependente (precisão) é contínua?
# Sim.

# Em terceiro, a variável independente (algoritmos) tem dois ou mais grupos de algoritmos?
# Sim, existem 6 algoritmos.

# Em quarto, as observações são dependentes dado que, de acordo com as precisões dos algoritmos alguns interligam-se com os outros
# isto é possível de ser observado através da matriz de correlações da alínea a) que mostra indíces em vários algortimos de interligação entre si
# o que poderá explicar a interligação nas amostras (o que vai ser muito importante depois a definir o teste que vamos utilizar!)

# Em quinto, as observações tem outliers significativos?
# Para tal criamos um boxplot, no qual verificamos se existem outliers fora dos bigodes.

preciSVM <- precisoes$SVM 
preciDT <- precisoes$DT 
preciKN <- precisoes$KN 
preciRF <- precisoes$RF  
preciML <- precisoes$ML 
preciGB <- precisoes$GB 

numPrecisoes=c("SVM","DT","KN","RF","ML","GB")

dadosAgrupados <- list(preciSVM, preciDT, preciKN, preciRF, preciML, preciGB)

boxplot(dadosAgrupados,
        names= numPrecisoes,main = 'Precisão de cada algoritmo', xlab = 'Algoritmos', ylab = 'Precisão')

# Após analisar o boxplot, foi possível confirmar que, de facto, existem outliers significativos,
# dado que a aceleração é uma variável contínua proveniente de uma medição.

# Contudo, será feita também a verificação através do cálculo dos valores dos limites inferiores e superiores
# Para tal, será necessário averiguar se existem valores fora da amplitude dos bigodes.
# Consequentemente, estes valores terão de ser calculados:

iqrSVMcyl <- IQR(preciSVM)
iqrDTcyl <- IQR(preciDT)
iqrKNcyl <- IQR(preciKN)
iqrRFcyl <- IQR(preciRF)
iqrMLcyl <- IQR(preciML)
iqrGBcyl <- IQR(preciGB)

medianSVMcyl <- median(preciSVM)
medianDTcyl <- median(preciDT)
medianKNcyl <- median(preciKN)
medianRFcyl <- median(preciRF)
medianMLcyl <- median(preciML)
medianGBcyl <- median(preciGB)

liminfSVM <- quantile(preciSVM, 0.25) - 1.5 * iqrSVMcyl
limsupSVM <- quantile(preciSVM, 0.75) + 1.5 * iqrSVMcyl

liminfDT <- quantile(preciDT, 0.25) - 1.5 * iqrDTcyl
limsupDT <- quantile(preciDT, 0.75) + 1.5 * iqrDTcyl

liminfKN <- quantile(preciKN, 0.25) - 1.5 * iqrKNcyl
limsupKN  <- quantile(preciKN, 0.75) + 1.5 * iqrKNcyl

liminfRF <- quantile(preciRF, 0.25) - 1.5 * iqrRFcyl
limsupRF <- quantile(preciRF, 0.75) + 1.5 * iqrRFcyl

liminfML <- quantile(preciML, 0.25) - 1.5 * iqrMLcyl
limsupML <- quantile(preciML, 0.75) + 1.5 * iqrMLcyl

liminfGB <- quantile(preciGB, 0.25) - 1.5 * iqrGBcyl
limsupGB  <- quantile(preciGB, 0.75) + 1.5 * iqrGBcyl

outliersSVMcyl <- preciSVM[preciSVM > limsupSVM | preciSVM < liminfSVM]

outliersDTcyl <- preciDT[preciDT > limsupDT | preciDT < liminfDT]

outliersKNcyl <- preciKN[preciKN > limsupKN | preciKN < liminfKN]

outliersRFcyl <- preciRF[preciRF > limsupRF | preciRF < liminfRF]

outliersMLcyl <- preciML[preciML > limsupML | preciML < liminfML]

outliersGBcyl <- preciGB[preciGB > limsupGB | preciGB < liminfGB]

# Em sexto, após averiguarmos que, de facto, existem outliers, não será possível usar testes ONE-WAY ANOVA, 
# vamos também averiguar que nao existe homogenidade de variâncias.

# Para realizarmos essa afirmação, usamos testes de Shapiro.

shapiro.test(precisoes$SVM)
shapiro.test(precisoes$DT)
shapiro.test(precisoes$KN)
shapiro.test(precisoes$RF)
shapiro.test(precisoes$ML)
shapiro.test(precisoes$GB)


# Para os algoritmos SVM, DT,KN,RF e GB, a homogenidade é verificada, visto que
# o nível de significância é superior a 0,05.

# Contudo, para o algoritmo ML, o mesmo não se veifica, visto que é menor que 0,05.
# Podemos então constantar que não existe homogenidade dos dados.

# Assim, podemos concluir que, como uma ou mais hipóteses falham, a variável dependente não é normalmente distribuída.
# Recorremos então a testes não paramétricos, visto que o uso de ANOVA é impossibilitado.

# Outra maneira de verificar isto seria a partir de lillie tests ou mesmo histogramas

hist(precisoes$SVM)
hist(precisoes$DT)
hist(precisoes$KN)
hist(precisoes$RF)
hist(precisoes$ML)
hist(precisoes$GB)

# Para conseguirmos então verificar qual das hipóteses se encontra correta, será necessário
# usar um teste de Kruskal-Wallis ou Friedman.

# Após a visualização dos dados foi possível deduzir que não se trata de uma distribuição normal
# então não podemos aplicar um teste paramétrico, mas sim testes não paramétricos

# Assim, podemos concluir que, como uma ou mais hipóteses falham, a variável dependente não é normalmente distribuída.
# Recorremos então a testes não paramétricos, visto que o uso de ANOVA é impossibilitado

# Para conseguirmos então verificar qual das hipóteses se encontra correta, será necessário
# usar um teste de Friedman uma vez que as amostras estão interligadas entre si

# Kruskal-Wallis poderia ser uma opção válida se os dados fossem independentes entre si, e como vimos antes a partir da alínea a) e 
# a explicação das variáveis, não podemos concluir que os dados são independentes

# O teste Friedman os dados são medidos em uma escala ordinal e as amostras são relacionadas, e, no nosso caso, esta situação é verificada e confirmada

# Vamos aplicar um estudo baseado num teste Friedman

matriz_precisoes <- as.matrix(precisoes)
friedman.test(matriz_precisoes)

# Como p-value = 0.1212 > 0.05 = alfa, não se rejeita H0.

# Não há evidências estatísticas para afirmar que existem diferenças significativas
# entre a precisão dos diferentes algoritmos.

# c)
# Como foi inferido na alínea anterior não existem evidências estatísticas que afirmem que existam diferenças significativas
# logo não é possível realizar um estudo post-hoc ao teste efetuado previamente

# Caso fosse possível uma alternativa válida seria o uso to TuskeyHSD, mas, neste caso, não é!