# Poderiamos usar uma planilha de dados
# E então fazer o passos básicos para ter acesso aos dados e verificar a organização do mesmo
library(readxl)

setwd("C:/Users/Usuário/Desktop")
antes.depois <- data.frame(read_excel("tabela.xlsx"))
antes.depois
str(antes.depois)

# Mas hoje usaremos um outra alternativa para variar um pouco

# Tipo 01 - Pareado / Dependentes
antes <- c(217, 252, 229, 200, 209, 213)
depois <- c(209, 241, 230, 208, 206, 211)

# 1º - Verificar a normalidade dos dados
# Interpretação - se p-value for maior que a significância indica que os dados vem de uma distribuição normal

# Hipóteses
# Ho: Dados estão normalmente distribuidos
# Ha: Dados não estão normalmente distribuidos
shapiro.test(antes)
shapiro.test(depois)

# 2º - Avaliando a independência dos dados via grafico de dispersão
plot(antes ~ depois, main = "Gráfico de dispersão")

# 3º - Aplique o teste t
# Interpretação - se p-value for MENOR que a significância indica haver diferença significativa

# Hipóteses 
# Ho: primeiro = segundo

# Ha: primeiro ??? segundo - quando anternative = "two.sided"
# ou Ha: primeiro > segundo - quando anternative = "greater"
# ou Ha: primeiro < segundo - quando anternative = "less"

t.test(antes, depois,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = TRUE)




# Tipo 02 - Não pareados / Independentes

# Caso 01 - Teste t para amostras independentes com variância HOMogêneas
probiotico <- c(129.9, 130.1, 131.6, 130.7, 129.0, 131.2, 130.2, 131.3)
placebo <- c(120.9, 118.9, 121.3, 121.2, 119.6, 118.7, 120.6, 120.8)

# 1º - Verificar a normalidade dos dados
# Interpretação  - se p-valor for maior que a significância estabelecida indica que os dados possuem uma distribuição normal

# Hipóteses 
# Ho: Dados estão normalmente distribuidos
# Ha: Dados não estão normalmente distribuidos

shapiro.test(probiotico)
shapiro.test(placebo)

# 2º - Avaliando a independência dos dados via grafico de dispersão
plot(probiotico ~ placebo, main = "Gráfico de dispersão")

# 3º - Avaliando a homogeneidade de variâncias
# Interpretação - se p-value for MAIOR que a significância indica variâncias iguais

# Hipóteses 
# Ho: Não há diferenças entre variâncias
# Ha: Há diferenças entre variâncias

var.test(probiotico, placebo,
         alternative = "two.sided",
         conf.level = 0.95)

# 4º - Aplicar o teste t
# Interpretação - se p-value for MENOR que a significância indica haver diferença significativa

# Hipóteses 
# Ho: primeiro = segundo

# Ha: primeiro ??? segundo - quando anternative = "two.sided"
# ou Ha: primeiro > segundo - quando anternative = "greater"
# ou Ha: primeiro < segundo - quando anternative = "less"

t.test(probiotico, placebo, 
       alternative = "two.sided", 
       var.equal = TRUE, 
       paired = FALSE,
       conf.level = 0.95)

# Caso 02 - Teste t para amostras independentes com variância HETerogêneas
dados01 <- c(129.9, 130.1, 131.6, 130.7, 129.0, 131.2, 130.2, 131.3)
dados02 <- c(330, 285, 280, 314, 310, 278, 290, 307)

# 1º - Verificar a normalidade dos dados
# Interpretação - se p-valor MAIOR que a significância estabelecida indica que os dados vem de uma distribuição normal

# Hipóteses 
# Ho: Dados estão normalmente distribuidos
# Ha: Dados não estão normalmente distribuidos
shapiro.test(dados01)
shapiro.test(dados02)

# 2º - Avaliando a independência dos dados via grafico de dispersão
plot(dados01 ~ dados02, main = "Gráfico de dispersão")


# 3º - Avaliando a homogeneidade de variâncias
# Um p-value maior que a significância indica variâncias iguais

# Interpretação - se p-value for MAIOR que a significância indica variâncias iguais

# Hipóteses 
# Ho: Não há diferenças entre variâncias
# Ha: Há diferenças entre variâncias
var.test(dados01, dados02,
         alternative = "two.sided",
         var.equal = FALSE, 
         conf.level = 0.95)

# 4º - Aplicar o teste t
# Interpretação: se p-value for MENOR que a significância indica haver diferença significativa
# Hipóteses 
# Ho: primeiro = segundo

# Ha: primeiro ??? segundo - quando anternative = "two.sided"
# ou Ha: primeiro > segundo - quando anternative = "greater"
# ou Ha: primeiro < segundo - quando anternative = "less"

t.test(dados01, dados02, 
       alternative = "two.sided", 
       var.equal = FALSE,
       paired = FALSE,
       conf.level = 0.95)



# Tipo 03 - O caso de uma amostra
conc <- c(1.10,1.18,1.01,0.99,1.12,1.15,1.10,1.05,1.10,1.00)

# 1º - Verificar a normalidade dos dados
# Interpretação - se p-valor for maior que a significância estabelecida indica que os dados vem de uma distribuição normal

# Hipóteses
# Ho: Dados estão normalmente distribuidos
# Ha: Dados não estão normalmente distribuidos
shapiro.test(conc)

# 2º Aplicar o teste t
# Interpretação: se p-value for MENOR que a significância indica haver diferença significativa

# Hipóteses 
# Ho: media amostral = média população

# Ha: media amostral ??? média população - quando anternative = "two.sided"
# ou Ha: media amostral > média população - quando anternative = "greater"
# ou Ha: media amostral < média população - quando anternative = "less"

t.test(conc, 
       mu = 1.11,
       alternative = "two.sided",
       conf.level = 0.95)