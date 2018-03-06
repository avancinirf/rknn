
### ---------------------------------------------------------------------------
### Título: Classificação Preditiva com algoritmo k-NN
### Descrição: Utilização do algoritmo k-NN para classificação preditiva em dados de diagnóstico de diabetes
### Autor: Ricardo Avancini
### e-mail: avancini.rf@gmail.com
### Repositório: https://github.com/avancinirf/rknn
### ---------------------------------------------------------------------------

# Weka DataSet
# http://www.technologyforge.net/Datasets/

# Verificando e instalando um único pacote
if (!require("class")) install.packages("class")
require('class')

# Selecionando o diretorio raiz para o script
setwd("C:/Users/Ricardo/Documents/SITE_POSTS/KNN/rknn")
#getwd()

# Carregando arquivo CSV com os dados de diagnóstico de diabetes
dados <- as.data.frame(read.table("data/DiabetesDiagnosis.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)) 

# Criando as variáveis para executar a função K-NN sem normalização nos dados
treinamento<-dados[1:708, 1:8]
rotulo<-dados[1:708, 9:9]
teste<-dados[709:768, 1:8]
# Exemplo de como executar o algoritmo k-NN com k =5
estimado <- as.data.frame(knn(treinamento, teste, rotulo, 5))

# Criação de um data.frame para armazenar resultado
resultado <-as.data.frame(dados[709:768, 9]) # Coluna com o resultado original para validar o algoritmo
resultado[,2]<-as.data.frame(estimado[, 1]) # Coluna com o resultado da predição sem a normalização


