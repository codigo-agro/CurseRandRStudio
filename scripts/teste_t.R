# Poderiamos usar uma planilha de dados
# E ent�o fazer o passos b�sicos para ter acesso aos dados e verificar a organiza��o do mesmo
library(readxl)

setwd("C:/Users/Usu�rio/Desktop")
antes.depois <- data.frame(read_excel("tabela.xlsx"))
antes.depois
str(antes.depois)

# Mas hoje usaremos um outra alternativa para variar um pouco

# Tipo 01 - Pareado / Dependentes
antes <- c(217, 252, 229, 200, 209, 213)
depois <- c(209, 241, 230, 208, 206, 211)

# 1� - Verificar a normalidade dos dados
# Interpreta��o - se p-value for maior que a signific�ncia indica que os dados vem de uma distribui��o normal

# Hip�teses
# Ho: Dados est�o normalmente distribuidos
# Ha: Dados n�o est�o normalmente distribuidos
shapiro.test(antes)
shapiro.test(depois)

# 2� - Avaliando a independ�ncia dos dados via grafico de dispers�o
plot(antes ~ depois, main = "Gr�fico de dispers�o")

# 3� - Aplique o teste t
# Interpreta��o - se p-value for MENOR que a signific�ncia indica haver diferen�a significativa

# Hip�teses 
# Ho: primeiro = segundo

# Ha: primeiro ??? segundo - quando anternative = "two.sided"
# ou Ha: primeiro > segundo - quando anternative = "greater"
# ou Ha: primeiro < segundo - quando anternative = "less"

t.test(antes, depois,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = TRUE)




# Tipo 02 - N�o pareados / Independentes

# Caso 01 - Teste t para amostras independentes com vari�ncia HOMog�neas
probiotico <- c(129.9, 130.1, 131.6, 130.7, 129.0, 131.2, 130.2, 131.3)
placebo <- c(120.9, 118.9, 121.3, 121.2, 119.6, 118.7, 120.6, 120.8)

# 1� - Verificar a normalidade dos dados
# Interpreta��o  - se p-valor for maior que a signific�ncia estabelecida indica que os dados possuem uma distribui��o normal

# Hip�teses 
# Ho: Dados est�o normalmente distribuidos
# Ha: Dados n�o est�o normalmente distribuidos

shapiro.test(probiotico)
shapiro.test(placebo)

# 2� - Avaliando a independ�ncia dos dados via grafico de dispers�o
plot(probiotico ~ placebo, main = "Gr�fico de dispers�o")

# 3� - Avaliando a homogeneidade de vari�ncias
# Interpreta��o - se p-value for MAIOR que a signific�ncia indica vari�ncias iguais

# Hip�teses 
# Ho: N�o h� diferen�as entre vari�ncias
# Ha: H� diferen�as entre vari�ncias

var.test(probiotico, placebo,
         alternative = "two.sided",
         conf.level = 0.95)

# 4� - Aplicar o teste t
# Interpreta��o - se p-value for MENOR que a signific�ncia indica haver diferen�a significativa

# Hip�teses 
# Ho: primeiro = segundo

# Ha: primeiro ??? segundo - quando anternative = "two.sided"
# ou Ha: primeiro > segundo - quando anternative = "greater"
# ou Ha: primeiro < segundo - quando anternative = "less"

t.test(probiotico, placebo, 
       alternative = "two.sided", 
       var.equal = TRUE, 
       paired = FALSE,
       conf.level = 0.95)

# Caso 02 - Teste t para amostras independentes com vari�ncia HETerog�neas
dados01 <- c(129.9, 130.1, 131.6, 130.7, 129.0, 131.2, 130.2, 131.3)
dados02 <- c(330, 285, 280, 314, 310, 278, 290, 307)

# 1� - Verificar a normalidade dos dados
# Interpreta��o - se p-valor MAIOR que a signific�ncia estabelecida indica que os dados vem de uma distribui��o normal

# Hip�teses 
# Ho: Dados est�o normalmente distribuidos
# Ha: Dados n�o est�o normalmente distribuidos
shapiro.test(dados01)
shapiro.test(dados02)

# 2� - Avaliando a independ�ncia dos dados via grafico de dispers�o
plot(dados01 ~ dados02, main = "Gr�fico de dispers�o")


# 3� - Avaliando a homogeneidade de vari�ncias
# Um p-value maior que a signific�ncia indica vari�ncias iguais

# Interpreta��o - se p-value for MAIOR que a signific�ncia indica vari�ncias iguais

# Hip�teses 
# Ho: N�o h� diferen�as entre vari�ncias
# Ha: H� diferen�as entre vari�ncias
var.test(dados01, dados02,
         alternative = "two.sided",
         var.equal = FALSE, 
         conf.level = 0.95)

# 4� - Aplicar o teste t
# Interpreta��o: se p-value for MENOR que a signific�ncia indica haver diferen�a significativa
# Hip�teses 
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

# 1� - Verificar a normalidade dos dados
# Interpreta��o - se p-valor for maior que a signific�ncia estabelecida indica que os dados vem de uma distribui��o normal

# Hip�teses
# Ho: Dados est�o normalmente distribuidos
# Ha: Dados n�o est�o normalmente distribuidos
shapiro.test(conc)

# 2� Aplicar o teste t
# Interpreta��o: se p-value for MENOR que a signific�ncia indica haver diferen�a significativa

# Hip�teses 
# Ho: media amostral = m�dia popula��o

# Ha: media amostral ??? m�dia popula��o - quando anternative = "two.sided"
# ou Ha: media amostral > m�dia popula��o - quando anternative = "greater"
# ou Ha: media amostral < m�dia popula��o - quando anternative = "less"

t.test(conc, 
       mu = 1.11,
       alternative = "two.sided",
       conf.level = 0.95)