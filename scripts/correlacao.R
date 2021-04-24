# Correlação de Pearson

# Usando carregando os pacotes para as aulas
library(ggplot2)                                                     # Pacote gráfico, é prerrequisito para o pacote seguinte
library(ggcorrplot)                                                  # Pacote principal para gerar a matriz gráfica
library(readxl)                                                      # Pacote para carregar arquivos .xlsx
library(writexl)                                                     # Pacotes para exportar data frames para formatos .xlsx

setwd("C:/Users/Usuário/Desktop")                                    # Indica-se o diretório com o arquivo de dados

dados = data.frame(read_excel("cars.xlsx"))                          # Carrega-se o arquivo de dados

dados                                                                # Visualizando os dados

str(dados)                                                           # Visualizando a estrutura dos dados


#------------------------------------------------------------------------------------------------------------------------------
# Forma 01 - Usar a função cor.test() - para duas variáveis quais quer
# Dentro da função indique duas variáveis e pronto
# Vc obterá o valor de correlação, a significância e algumas outras informações a mais
cor.test(dados$mpg,  
         dados$hp)


#------------------------------------------------------------------------------------------------------------------------------
# Forma 02 - Obter ambas os resultados (tanto as correlações quando as significâncias)
# e exportar como uma planilha excel

# As correlações
matriz.cor <- data.frame(cor(dados[2:length(dados)]))

# E as sinificâncias
valores_p <- data.frame(cor_pmat(dados[2:length(dados)]))

# Agora vamos pegar estas duas tabelas e exportar para um formato .xlsx
# 
write_xlsx(list(valores_p, matriz.cor),        # indicamos para a função write_xlsx() que temos uma lista de data.frame usando list()
           "teste_de_correlacao.xlsx",         # damos um nome para o arquivo .xlsx que será gerada
           format_headers = TRUE)              # pedimos que o cabeçalho das nossas planilhas seja negritado


#------------------------------------------------------------------------------------------------------------------------------
# Forma 03 - Criando uma matriz gráfica de correlação

# 1º Obtemos os dados de correlação usanddo a função cor()
# Dentro da função cor(indicamos quais as colunas de dados 
# que quero que sejam usadas para os calculos
# vamos armazenar o resultado (que é uma matriz de correlações) 
# numa variavel que chamo aqui de matriz.cor
# mas que vc pode ficar a vontade para usar qualquer outro nome

# onde está escrito dados[2:length(dados)
# estamos indicando quais coluna do conjunto de dados do arquivo "dados" queremos que
# sejam usadas para realizar os calculos de correlação
# "2" é a coluna inicial. Após ":" podiamos colocar um outro número indicando a coluna final
# Como desejamos que sejam usadas todos as demais colunas, então usamos a função length()
# ao introduzirmos dentro dela o nome do nosso arquivo de dados ela retorna o valor
# correspondente ao total de coluna do nosso arquivo. Por exemplo, se o arquivo tem
# 10 colunas, "length(dados)" é o mesmo que digitar o número "10" apos os ":"

matriz.cor <- cor(dados[2:length(dados)])                        

# Digitando o nome da variável podemos visualizar essa matriz de resultados/correlações
matriz.cor

# 2º - Vamos obter os valores de significância das correlações
# muito semelhando ao que fizemos para obter apenas as correlação:
sig.cor <- cor_pmat(dados[2:length(dados)])

sig.cor

# 3º - Agora basta editarmos a matriz de correlação
ggcorrplot(matriz.cor,                                           # Arquivo contendo os dados para correlação
           hc.order = TRUE,                                      # Se TRUE, a matriz será ordenada/organizada
           method = "square",                                    # Qual formato geométrico deve ser usado para pintar a matriz
           type = "lower",                                       # Altera o jeitão da matriz
           lab = TRUE,                                           # Se TRUE os valores de correlação aparecem na matriz grafica
           digits = 2,                                           # Digitos após a virgula
           lab_size = 3,                                         # Tamanho da fonte dos números
           p.mat = sig.cor,                                      # Indica se as ediação referentes as significÂncia devem ser consideradas
           sig.level = 0.01,                                     # Nível de significância que a função deverá considerar
           insig = "blank",                                      # O que não entrar no nível de sinificancia ficará em branco
           colors = c("darkgoldenrod1","darkred","darkgreen"))   # Indicamos três para serem usadas na matriz gráfica
