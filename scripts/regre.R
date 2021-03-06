# ----------------------------------------------------
# PREPARANDO O AMBIENTE PARA INICIAR OS C�LCULOS
# ----------------------------------------------------

# 01 - Carregando os pacotes que iremos usar
library(readxl)

# 02 - Indicando o diret�rio de trabalho
setwd("C:/Users/Usu�rio/Desktop")

# 03 - Indicando o arquivo de dados
dados <- data.frame(read_excel("regre.xlsx"))

# 04 - Vizualizando o conjunto de dados
head(dados)
print(dados)

# 05 - Examinando a estrutura do mesmo
str(dados)

# 06 - Armazenar a vari�vel numa vari�vel nova
var <- dados$mspa

# 07 - Armazenando as m�dias da vari�vel dependente
medias_var <- tapply(var, dados$trat, mean)
medias_var

# 08 - Armazenando as m�dias da vari�vel independente
medias_doses <- tapply(dados$trat, dados$trat, mean)
medias_doses


# ----------------------------------------------------
# ----------------------------------------------------
# VISUALIZANDO O COMPORTAMENTO GR�FICO DOS DADOS
# ----------------------------------------------------
# ----------------------------------------------------

plot(dados$trat, 
     var, 
     ylim=c(min(var)-1, max(var)))

par(new = TRUE)

plot(medias_doses, 
     medias_var, 
     ylim=c(min(var)-1, max(var)), 
     xlab = "",
     ylab = "",
     pch = 19,
     col = "blue",
     axes = FALSE)


# ----------------------------------------------------
# ----------------------------------------------------
# Testando os modelos de regress�o
# ----------------------------------------------------
# ----------------------------------------------------

# Criar o modelo linear
lin <- lm(medias_var ~ medias_doses)

# Verificando as estat�sticas do modelo
summary(lin)

# Criando o modelo quadr�tico
qua <- lm(medias_var ~ medias_doses + I(medias_doses^2))

# Verificando as estat�sticas do modelo
summary(qua)


# ----------------------------------------------------
# ----------------------------------------------------
# AVALIANDO OS PRESSUPOSTOS PARA OS MODELOS
# ----------------------------------------------------
# ----------------------------------------------------


# ----------------------------------------------------
# 01 - Signific�ncia do coeficiente
# ----------------------------------------------------

# Modelo Linear
anova(lin)

# Modelo quadr�tico
anova(qua)

# ----------------------------------------------------
# 02 - Normalidade dos res�duos
# ----------------------------------------------------

# Modelo Linear
shapiro.test(lin$residuals)


# Modelo quadr�tico
shapiro.test(qua$residuals)


# ----------------------------------------------------
# 03 - Linearidade - para o modelo linear
# ----------------------------------------------------

cor.test(var, dados$trat)

# ----------------------------------------------------
# 04 - Homocedasticidade
# ----------------------------------------------------

# Modelo linear
plot(rstudent(lin) ~ fitted(lin), 
     pch = 19)

abline(h = 0, 
       lty = 2, 
       col = "red")


# Modelo quadr�tico
plot(rstudent(qua) ~ fitted(qua), 
     pch = 19)

abline(h = 0, 
       lty = 2, 
       col = "red")


# ----------------------------------------------------
# Extra - podemos encontrar o valor m�ximo ou m�nimo de uma fun��o quadr�tica
# ----------------------------------------------------

# ----------------------------------------------------
# Modo 01 - X(max ou min) = -b/2a
# ----------------------------------------------------

c = qua$coefficients[1]

b = qua$coefficients[2]

a = qua$coefficients[3]

ponto.extremo = - (b / (2 * a))

print(ponto.extremo)

# ----------------------------------------------------
# Modo 02 - usando a fun��o optimize
# ----------------------------------------------------
fx <- function(x) qua$coefficients[1] + qua$coefficients[2]*x + qua$coefficients[3]*x^2

optimize(fx, c(min(medias_doses), max(medias_doses)), maximum=T)




