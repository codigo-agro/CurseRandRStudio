# Correla��o de Pearson

# Usando carregando os pacotes para as aulas
library(ggplot2)                                                     # Pacote gr�fico, � prerrequisito para o pacote seguinte
library(ggcorrplot)                                                  # Pacote principal para gerar a matriz gr�fica
library(readxl)                                                      # Pacote para carregar arquivos .xlsx
library(writexl)                                                     # Pacotes para exportar data frames para formatos .xlsx

setwd("C:/Users/Usu�rio/Desktop")                                    # Indica-se o diret�rio com o arquivo de dados

dados = data.frame(read_excel("cars.xlsx"))                          # Carrega-se o arquivo de dados

dados                                                                # Visualizando os dados

str(dados)                                                           # Visualizando a estrutura dos dados


#------------------------------------------------------------------------------------------------------------------------------
# Forma 01 - Usar a fun��o cor.test() - para duas vari�veis quais quer
# Dentro da fun��o indique duas vari�veis e pronto
# Vc obter� o valor de correla��o, a signific�ncia e algumas outras informa��es a mais
cor.test(dados$mpg,  
         dados$hp)


#------------------------------------------------------------------------------------------------------------------------------
# Forma 02 - Obter ambas os resultados (tanto as correla��es quando as signific�ncias)
# e exportar como uma planilha excel

# As correla��es
matriz.cor <- data.frame(cor(dados[2:length(dados)]))

# E as sinific�ncias
valores_p <- data.frame(cor_pmat(dados[2:length(dados)]))

# Agora vamos pegar estas duas tabelas e exportar para um formato .xlsx
# 
write_xlsx(list(valores_p, matriz.cor),        # indicamos para a fun��o write_xlsx() que temos uma lista de data.frame usando list()
           "teste_de_correlacao.xlsx",         # damos um nome para o arquivo .xlsx que ser� gerada
           format_headers = TRUE)              # pedimos que o cabe�alho das nossas planilhas seja negritado


#------------------------------------------------------------------------------------------------------------------------------
# Forma 03 - Criando uma matriz gr�fica de correla��o

# 1� Obtemos os dados de correla��o usanddo a fun��o cor()
# Dentro da fun��o cor(indicamos quais as colunas de dados 
# que quero que sejam usadas para os calculos
# vamos armazenar o resultado (que � uma matriz de correla��es) 
# numa variavel que chamo aqui de matriz.cor
# mas que vc pode ficar a vontade para usar qualquer outro nome

# onde est� escrito dados[2:length(dados)
# estamos indicando quais coluna do conjunto de dados do arquivo "dados" queremos que
# sejam usadas para realizar os calculos de correla��o
# "2" � a coluna inicial. Ap�s ":" podiamos colocar um outro n�mero indicando a coluna final
# Como desejamos que sejam usadas todos as demais colunas, ent�o usamos a fun��o length()
# ao introduzirmos dentro dela o nome do nosso arquivo de dados ela retorna o valor
# correspondente ao total de coluna do nosso arquivo. Por exemplo, se o arquivo tem
# 10 colunas, "length(dados)" � o mesmo que digitar o n�mero "10" apos os ":"

matriz.cor <- cor(dados[2:length(dados)])                        

# Digitando o nome da vari�vel podemos visualizar essa matriz de resultados/correla��es
matriz.cor

# 2� - Vamos obter os valores de signific�ncia das correla��es
# muito semelhando ao que fizemos para obter apenas as correla��o:
sig.cor <- cor_pmat(dados[2:length(dados)])

sig.cor

# 3� - Agora basta editarmos a matriz de correla��o
ggcorrplot(matriz.cor,                                           # Arquivo contendo os dados para correla��o
           hc.order = TRUE,                                      # Se TRUE, a matriz ser� ordenada/organizada
           method = "square",                                    # Qual formato geom�trico deve ser usado para pintar a matriz
           type = "lower",                                       # Altera o jeit�o da matriz
           lab = TRUE,                                           # Se TRUE os valores de correla��o aparecem na matriz grafica
           digits = 2,                                           # Digitos ap�s a virgula
           lab_size = 3,                                         # Tamanho da fonte dos n�meros
           p.mat = sig.cor,                                      # Indica se as edia��o referentes as signific�ncia devem ser consideradas
           sig.level = 0.01,                                     # N�vel de signific�ncia que a fun��o dever� considerar
           insig = "blank",                                      # O que n�o entrar no n�vel de sinificancia ficar� em branco
           colors = c("darkgoldenrod1","darkred","darkgreen"))   # Indicamos tr�s para serem usadas na matriz gr�fica
