#carregando pacotes necessários
.libPaths("C:/Users/mathg/OneDrive/Documents/Diretório/Pacotes do R")
install.packages("tidyr")
install.packages("tseries")
install.packages("ggplot2")
install.packages('lmtest')
install.packages('sandwich')
install.packages(c("tm", "wordcloud", "SnowballC"))
install.packages('stargazer')
install.packages('broom')
install.packages('withr')
install.packages('robust')
install.packages("caret")
install.packages('mlr')
library(mlr)
library(reticulate)
library(caret)
library(readr)
library(dplyr)
library(ggplot2)
library(tseries)
library(lmtest)
library(sandwich)
library(tm)
library(wordcloud)
library(SnowballC)
library(stargazer)
library(broom)
library(withr)
library(robust)

#Definindo o diretório
setwd("C:/Users/mathg/OneDrive/Documents/Diretório/projeto 1")

#Selecionando a base de dados separado por vírgula 
base <- read.csv("teste_indicium_precificacao.csv", sep = ',')

#Verficicando a estrutura dos dados
str(base)

# verificando se existe valores ausentes
sapply(base, function(x) sum(is.na(x)))


# transformando alguns dados para o tipo correto
base$bairro_group <- factor(base$bairro_group)
base$bairro <- factor(base$bairro)                
base$room_type <- factor(base$room_type)
base$ultima_review <- as.Date(base$ultima_review)

# verificando se há anúncios sobre o mesmo imóvel
base %>% count(bairro, bairro_group, latitude, longitude, host_id) %>% filter(n>1)

# estatística sumária
summary(base)

# gráfico de caixa para vermos como está distribuído as obserções 
boxplot(base$minimo_noites)

# medidas de dispersão para variáveis relevantes
sd(base$price)
var(base$price)

# qual bairro é mais caro?

# gráfico de pontos
ggplot(
  data = base,
  mapping = aes(
    x = bairro_group,
    y = price)) + geom_point()

#gráfico de preço médio por bairro
sumario <- base %>%
  group_by(bairro_group) %>%
  summarize(preço_medio = mean(price))

ggplot(data = sumario, aes(x = bairro_group, y = preço_medio)) +
  geom_bar(stat = "identity")

# o quanto o bairro influencia o preço?

# valores absolutos
reg1 <- lm(price~bairro_group, data=base)
summary(reg1)

#verificando a robustez do modelo

#normalidade
res1 <- resid(reg1) 
jarque.bera.test(res1)

#excluindo outliers
zscore <- scale(base$price)
limitez <- 3
base2 <- base[abs(zscore) <= limitez, ]

#rodando o modelo sem outliers
reg2 <- lm(price~bairro_group, data=base2)
summary(reg2)

#verificando a homocedasticidade
bptest(reg2)

# corrigindo a heterocedasticidade e a autocorrelação residual
coeftest(reg2, vcov. = vcovHC(reg2))


###################################################################################################
### respondendo a pergunta:qual a melhor localização para investir na compra de um imóvel?

base3 <- base2 #criando backup 

#atribuindo numero para os níveis da variável bairro  
base3 <- base3 %>% mutate(as.integer(base3$bairro))
colnames(base3)[17] <- 'numbairro'

#atribuindo numero para os níveis da variavel bairro_group
base3 <- base3 %>% mutate(as.integer(base3$bairro_group))
colnames(base3)[18] <- 'numbairro_group'

# os 25% valores mais altos de reviews por mes
base5 <- base3[base3$reviews_por_mes >
                                     quantile(base3$reviews_por_mes, 
                                              probs = 0.80, na.rm = TRUE), ]

# valores que o preço estejam entre 100 e 110

base5 <- base5[!is.na(base5$price) & base5$price >= 100 
                          & base5$price <= 110, ]

#qual bairro é mais demandado anunciado?
hist(base5$numbairro_group)

#extraindo apenas imoveis que são de manhattan e brooklyn

base5 <- base5[base5$bairro_group=="Brooklyn" | base5$bairro_group=="Manhattan",]

# qual area é a mais ofertada?
hist(base5$numbairro)
table(base5$bairro)

############################################################################################

# Price é influenciado por minimo_noites e disponibilidade_365?

reg5 <- lm(price~minimo_noites+disponibilidade_365, data=base2)
summary(reg5)
res5 <- resid(reg5)
jarque.bera.test(res5)
bptest(reg5)
coeftest(reg5, vcov. = vcovHC(reg5))

#######################################################################################

#Existe padrão no nome do anúncio dos aluguéis mais caros?

corpus <- Corpus(VectorSource(base5$nome))

#padronizar a escrita
corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)             
corpus <- tm_map(corpus, removeNumbers)                 
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))  
corpus <- tm_map(corpus, stripWhitespace)               

#criando uma matriz que representa a frequência das palavras
matriz_termo_documento <- DocumentTermMatrix(corpus)

#calculando a frequência total 
frequencia_palavras <- colSums(as.matrix(matriz_termo_documento))

#criando um df com as frequências
df_frequencia <- data.frame(Palavra = names(frequencia_palavras), Frequencia = frequencia_palavras)

#ordenando em ordem decrescente
df_frequencia <- df_frequencia[order(df_frequencia$Frequencia, decreasing = TRUE), ]

#as dez palavras que mais aparecem:
print(head(df_frequencia, 10))

######################################################################################################

#previsão do preço

#Transformando as varíaveis em logaritimo

loggY <- log(base3$price)
loggX1 <- log(base3$minimo_noites)
loggX2 <- log(base$disponibilidade_365)
loggX3 <- log(base3$reviews_por_mes)

#rodando a regressão
reg7 <- lm(price~minimo_noites+disponibilidade_365+host_id+
             bairro_group+ bairro+ reviews_por_mes+
             room_type, data=base3)
summary(reg7)

########################################################################################################

#novos dados

obsteste <- data.frame(id=2595,
                       nome='Skylit Midtown Castle',
                       host_id=2845,
                       host_name='Jennifer',
                       bairro_group='Manhattan',
                       bairro='Midtown',
                       room_type='Entire home/apt',
                       minimo_noites=1,
                       numero_de_reviews=45,
                       reviews_por_mes=0.38,
                       disponibilidade_365=355
                       )

#previsao
nova_previsao <- predict(reg7, newdata = obsteste)
nova_previsao 

# salvando o modelo em pkl
saveRDS(reg7, file = "PrevPreço.pkl")
