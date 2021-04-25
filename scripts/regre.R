# ----------------------------------------------------
# PREPARANDO O AMBIENTE PARA INICIAR OS CÁLCULOS
# ----------------------------------------------------

# 01 - Carregando os pacotes que iremos usar
library(readxl)

# 02 - Indicando o diretório de trabalho
setwd("C:/Users/Usuário/Desktop")

# 03 - Indicando o arquivo de dados
dados <- data.frame(read_excel("regre.xlsx"))

# 04 - Vizualizando o conjunto de dados
head(dados)
print(dados)

# 05 - Examinando a estrutura do mesmo
str(dados)

# 06 - Armazenar a variável numa variável nova
var <- dados$msr

# 07 - Armazenando as médias da variável dependente
medias_var <- tapply(var, dados$trat, mean)
medias_var

# 08 - Armazenando as médias da variável independente
medias_doses <- tapply(dados$trat, dados$trat, mean)
medias_doses


# ----------------------------------------------------
# ----------------------------------------------------
# VISUALIZANDO O COMPORTAMENTO GRÁFICO DOS DADOS
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
# Testando os modelos de regressão
# ----------------------------------------------------
# ----------------------------------------------------

# Criar o modelo linear
lin <- lm(medias_var ~ medias_doses)

# Verificando as estatísticas do modelo
summary(lin)

# Criando o modelo quadrático
qua <- lm(medias_var ~ medias_doses + I(medias_doses^2))

# Verificando as estatísticas do modelo
summary(qua)


# ----------------------------------------------------
# ----------------------------------------------------
# AVALIANDO OS PRESSUPOSTOS PARA OS MODELOS
# ----------------------------------------------------
# ----------------------------------------------------

# ----------------------------------------------------
# 01 - Significância do coeficiente
# ----------------------------------------------------

# Modelo Linear
anova(lin)

# Modelo quadrático
anova(qua)

# ----------------------------------------------------
# 02 - Normalidade dos dados
# ----------------------------------------------------

# Modelo Linear
shapiro.test(lin$residuals)


# Modelo quadrático
shapiro.test(qua$residuals)


# ----------------------------------------------------
# 03 - Linearidade - para o modelo linear
# ----------------------------------------------------

cor(var, dados$trat)

# ----------------------------------------------------
# 04 - Homocedasticidade
# ----------------------------------------------------

# Modelo linear
plot(rstudent(lin) ~ fitted(lin), 
     pch = 19)

abline(h = 0, 
       lty = 2, 
       col = "red")


# Modelo quadrático
plot(rstudent(qua) ~ fitted(qua), 
     pch = 19)

abline(h = 0, 
       lty = 2, 
       col = "red")