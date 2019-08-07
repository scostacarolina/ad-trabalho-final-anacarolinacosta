# ana carolina costa
# analise de dados
# trabalho final

# vd: indice de conservadorismo

#  hipotese: mulheres tendem a ser menos conservadores 
# hinterativva: educacao afeta o efeito de g?nero sobre o progressimo.

# variavel de controle: sexo, escolaridade, renda, idade, ra?a , pa?s
setwd("C:/Users/Carol/Desktop/cadeiras-mestrado/ad-davimoreira/ad-trabalhofinal")

# pre processamento de dados
library(foreign)
library(readstata13)
library(ggplot2)
library(RColorBrewer)
# solicitando abertura das bases de dados ####

argentina17 <-read.dta("argentina2017.dta")

bolivia17 <- read.dta("bolivia2017.dta")

brasil17 <- read.dta("brasil2017.dta")

chile17 <- read.dta("chile2017.dta")

colombia17 <- read.dta ("colombia2016.dta")

costarica17 <- read.dta("costarica2016.dta")

equador17 <- read.dta("equador2016.dta")

guiana17 <- read.dta13("guiana2016.dta")

paraguai17 <- read.dta("paraguai2016.dta")

peru17 <- read.dta("peru2017.dta")

uruguai17 <- read.dta("uruguai2017.dta")

venezuela17 <- read.dta("venezuela2016.dta")

# 


# corrigindo variavel Q10NEW ####

head(brasil17$q10new)
str(brasil17$q10new)
table(brasil17$q10new)
# alterando a vq10new para o brasil
brasil17$q10new <- as.numeric(droplevels(brasil17$q10new))

# alterando a vq10new para a argentina

argentina17$q10new <- as.numeric(droplevels(argentina17$q10new))

# alterando a vq10new para bolivia

bolivia17$q10new <- as.numeric(droplevels(bolivia17$q10new))

# alterando a vq10new para chile

chile17$q10new <- as.numeric(droplevels(chile17$q10new))

# alterando a vq10new para colombia
colombia17$q10new <- as.numeric(droplevels(colombia17$q10new))

# alterando a vq10new para costa rica

costarica17$q10new <- as.numeric(droplevels(costarica17$q10new))

# alterando a vq10new para equador

equador17$q10new <- as.numeric(droplevels(equador17$q10new))

# alterando a vq10new para guiana

guiana17$q10new <- as.numeric(droplevels(guiana17$q10new))

# alterando a vq10new para paraguai

paraguai17$q10new <- as.numeric(droplevels(paraguai17$q10new))

# alterando a vq10new para peru

peru17$q10new <- as.numeric(droplevels(peru17$q10new))

# alterando a vq10new para uruguai

uruguai17$q10new <- as.numeric(droplevels(uruguai17$q10new))

# alterando a vq10new para venezuela

venezuela17$q10new <- as.numeric(droplevels(venezuela17$q10new))



head(brasil17$q10new)
table(brasil17$q10new)                                           
# filtrando as variaveis necessarias para o modelo de cada subset ####
vars <- c("pais", "w14a", "d5", "d6", "q1", "q2", "etid", "q10new", "ed")

brasil17 <- brasil17[,vars]

argentina17 <- argentina17[,vars]

bolivia17<- bolivia17[,vars]

chile17<- chile17[,vars]

colombia17<- colombia17[,vars]

costarica17<- costarica17[,vars]

equador17<- equador17[,vars]

guiana17<- guiana17[,vars]

paraguai17<- paraguai17[,vars]

peru17<- peru17[,vars]

uruguai17<- uruguai17[,vars]

venezuela17<- venezuela17[,vars]

# juntar as bases de dados

datasetfinal <- rbind(brasil17,argentina17)

datasetfinal <- rbind(bolivia17,datasetfinal)

datasetfinal <- rbind(chile17,datasetfinal)

datasetfinal <- rbind(colombia17,datasetfinal)

datasetfinal <- rbind(costarica17,datasetfinal)

datasetfinal <- rbind(equador17,datasetfinal)

datasetfinal <- rbind(paraguai17,datasetfinal)

datasetfinal <- rbind(peru17,datasetfinal)

datasetfinal <- rbind(uruguai17,datasetfinal)

datasetfinal <- rbind(venezuela17,datasetfinal)

# verificando se est? tudo certo com o novo subset

dim(datasetfinal)



# observar/ tratar/ transformar as variaveis ####

vars

# observar variavel pais
head(datasetfinal$pais)
str(datasetfinal$pais)

# observando variavel w14a
head(datasetfinal$w14a)
str(datasetfinal$w14a)

table(datasetfinal$w14a)
# criando variavel dummy para W14

datasetfinal$aborto <- ifelse(datasetfinal$w14a == "Yes, it is justified", 1 , 0 ) 
# observando a nova variavek

head(datasetfinal$aborto)
str(datasetfinal$aborto)

# a partir de agora a variavel utilizada no modelo ser? "aborto"

# obsrvar variavel d5

head(datasetfinal$d5)
str(datasetfinal$d5)

summary(datasetfinal$d5)


#observar variavel d6

head(datasetfinal$d6)
str(datasetfinal$d6)

# observar a variavel renda

head(datasetfinal$q10new)
str(datasetfinal$q10new)

vars
# observar a variavel q1 - sexo


head(datasetfinal$q1)
str(datasetfinal$q1)

# tratar a variavel para transforma-la em dummy

datasetfinal$gen <- ifelse(datasetfinal$q1 == "Female", 1 , 0 )

# verificando se deu certo

head(datasetfinal$gen)

str(datasetfinal$gen)

# a partir de agora a variavel que sera utilizada no modelo ? "gen"

# observar a variavel q2 - idade

head(datasetfinal$q2)
str(datasetfinal$q2)

# observar a variavel etid

head(datasetfinal$etid)
str(datasetfinal$etid)
# criando uma segunda variavel dummy para definir ra?a
# 1 para branco, 0 para n?o branco

datasetfinal$etnia <- ifelse(datasetfinal$etid == "White", 1 , 0 )
# verificando se est? tudo certo com a nova variavel
# a partir de agora a variavel utilizada  no modelo sera "etnia"

head(datasetfinal$etnia)
str(datasetfinal$etnia)

# observar a variavel ed

head(datasetfinal$ed)
str(datasetfinal$ed)


# criando a variavel indice de  progressismo  para formar o modelo ####

datasetfinal$prog <- datasetfinal$d5 + datasetfinal$d6 + datasetfinal$aborto

datasetfinal$prog
head(datasetfinal$prog)
summary(datasetfinal$prog)

# graficos de frequencia de dummys ####

library(ggplot2)

# grafico sobre o posicionamento sobre aborto ####
table(datasetfinal$aborto)
# criando data frame para o grafico
g.aborto <- data.frame(posicao = c("Contra", " A favor"), qntd = c(6469,10872))

ggplot(g.aborto, aes(y = qntd, x = posicao )) + 
  geom_bar(stat = "identity", fill = c("lightcoral", "lightblue")) + 
  labs(y = "Total", x = "Posicionamento",title = "Posicionameto sobre Aborto em caso de risco para Gestante") + theme_classic (base_size = 16,base_family = 'serif')

# grafico de distribui??o de,0 5 ra?a na pesquisa ####

table(datasetfinal$etnia)

# criando a datafrane para o grafico

g.etnia <- data.frame(Cor = c("Branco", "Nao Branco"), qntd = c(5771,11545))

ggplot(g.etnia, aes(y=qntd, x = Cor)) + 
  geom_bar(stat = "identity", fill = c("tan", "tan4")) +
  labs ( y = "Total", x = "Cor", title = " Distribui??o dos entrevistados por Cor") + theme_classic (base_size = 16,base_family = 'serif')



# histograma de distribui??o dos anos escolares da amostra populacional

## Criando dataframe para histograma
t.educacao <- table(datasetfinal$ed) #salvando tabela de frequencia

# Definindo dataframe e suas variáveis
g.educacao <- data.frame(rotulos = names(t.educacao), # rotulo 
                         frequencia = c(t.educacao),  # frequencia
                         ordem = 1:length(t.educacao))# ordem dos rotulos

# Reordenando os rotulos para ficarem na ordem crescente
g.educacao$rotulos <- reorder(g.educacao$rotulos, 
                              g.educacao$ordem)

ggplot(g.educacao, aes(x = rotulos, y = frequencia)) + # componentes elementares
  geom_histogram(stat = "identity", fill = c("dodgerblue2")) + # definindo grafico 
  labs(y = "Frequ?ncia", x = "") + # rotulos 
  theme_classic(base_size = 16,        # definindo trabalho da letra
                base_family = 'serif') # definindo tipo da letra

## criando dataframe para histograma de renda

t.renda <- table(datasetfinal$q10new)

# definindo df e suas variaveis
g.renda <- data.frame(rotulos = names(t.renda), frequencia = c(t.renda), ordem = 1:length(t.renda))  

# fazer a ordenação do grafico
g.renda$rotulos <- reorder(g.renda$rotulos, g.renda$ordem)

# gerando o grafico
ggplot(g.renda, aes(x= rotulos, y = frequencia)) + geom_histogram(stat = "identity") + theme_classic()


# criando histograma para apresentar a variavel d.5

t.d5 <- table(datasetfinal$d5)

# definindo o df e suas variaveis

g.d5 <- data.frame(rotulos=names(t.d5), frequencia = c(t.d5), ordem = 1:length(t.d5))

# ordendando o grafico
g.d5$rotulos <- reorder(g.d5$rotulos, g.d5$ordem)

# solicitando o grafico
ggplot(g.d5, aes(x = rotulos, y = frequencia)) + geom_histogram(stat = "identity") + theme_classic()

# criando histograma para apresentar a variavel d.6

t.d6<- table(datasetfinal$d6)

# definindo o df e suas variaveis

g.d6 <- data.frame(rotulos=names(t.d6), frequencia = c(t.d6), ordem = 1:length(t.d6))

# reordenar o grafico

g.d6$rotulos <- reorder(g.d6$rotulos, g.d6$ordem)

# solicitando o grafico

ggplot(g.d6, aes(x=rotulos, y=frequencia)) + geom_histogram(stat = "identity") + theme_classic()


#  criando graficos bivariados

# criando a tabela

t.abortogen <- table(datasetfinal$aborto, datasetfinal$gen)

t.abortogen

# definindo o nome das linhas da tabela
row.names(t.abortogen) <- c("contra", "a favor")

t.abortogen

# definindo o nome das colunas da tabela

colnames(t.abortogen) <- c("homem", "mulher")

t.abortogen

# crinado o df para o graficp

g.abortogen <- data.frame( aborto = c("contra aborto","a favor aborto", "contra aborto", "a favor aborto"),
                           sexo = c("homem","homem", "mulher", "mulher"),
                           frequencia = c(3170, 5452, 3299, 5420))

# gerar o grafico

ggplot(g.abortogen, aes(x= sexo, y = frequencia, fill=aborto)) + geom_bar(stat = "identity") + theme_classic()

# aceitação do aborto por raça

# criando a tabela 
t.abortetnia <- table(datasetfinal$aborto, datasetfinal$etnia)

t.abortetnia
# colocando o nome das linhas da tabela
row.names(t.abortetnia) <- c("contra", "aceita")

t.abortetnia

# colocando o nome das colunas

colnames(t.abortetnia) <- c("não branco", "branco")

t.abortetnia

# ajustanto os valores para porcentagens
t.abortetnia[,"não branco"] <- t.abortetnia[,"não branco"] / sum(t.abortetnia[,"não branco"]) 
t.abortetnia[,"branco"] <- t.abortetnia[, "branco"] /sum(t.abortetnia[,"branco"])

t.abortetnia
# construindo o dataframe para o graficp

g.abortetnia <- data.frame(aborto = c("contra aborto", "aceita aborto", "contra aborto", "aceita aborto"),
                           cor = c("não branco", "não branco", "branco", "branco"), 
                           frequencia = c(37.31, 62.68,36.54,63.45))

 
# gerar o grafico

ggplot(g.abortetnia, aes(x = cor,y = frequencia, fill = aborto)) + geom_bar(stat = "identity") + theme_classic()

# gerando boxplots

g.brenda <- data.frame( renda = datasetfinal$q10new)

ggplot(g.brenda, aes(y = renda )) + geom_boxplot() + theme_classic() 


# boxplot de anos de educação

g.bed <- data.frame(educ = datasetfinal$ed)

ggplot(g.bed, aes(y= educ)) + geom_boxplot() + theme_classic()

# boxplot bivariado 
# anos de educação por gênero
g.gened <- data.frame(educ = datasetfinal$ed, gen = datasetfinal$gen)
g.gened$gen <- ifelse(g.gened$gen == 1, "Mulher", "Homem")
g.gened <- na.omit(g.gened)

ggplot(g.gened, aes(y =educ, x = gen)) + geom_boxplot() + theme_classic()



# criando o modelo de regressão  
reg01 <- lm(data= datasetfinal, prog ~ gen + ed + q2 + etnia + q10new + pais )


summary(reg01)
confint(reg01)


plot(reg01,which = 1)

residuals(reg01)

predict(reg01)

dreg01 <- data.frame(residuos = residuals(reg01), preditos = predict(reg01))

ggplot(dreg01, aes(x = preditos, y = residuos)) + geom_point() + geom_abline(slope = 0, intercept = 0) + theme_classic()
