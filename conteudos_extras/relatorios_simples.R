# DBC

library(ExpDes.pt)
library(readxl)

getwd()
setwd("C:/Users/Usuário/Desktop")

sink("C:/Users/Usuário/Desktop/dbc.txt")

dados = data.frame(read_excel("Exer03.xlsx"))

cat('\n')
cat('---------------------\n')
cat('Conjunto de dados\n')
cat('---------------------\n')
cat('\n')

dados

cat('\n')
cat('---------------------------------\n')
cat('Estrutura do conjunto de dados\n')
cat('---------------------------------\n')
cat('\n')

str(dados)

cat('\n')
cat('---------------------\n')
cat('ANOVA e Teste de Tukey\n')
cat('---------------------\n')
cat('\n')

dbc(dados$variedade,
    dados$bloco,
    dados$prod,
    quali = TRUE,
    mcomp = "tukey",
    sigT = 0.05,
    sigF = 0.05)


sink()


  
