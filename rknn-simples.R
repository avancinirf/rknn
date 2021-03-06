
### ---------------------------------------------------------------------------
### T�tulo: Classifica��o Preditiva com algoritmo k-NN
### Descri��o: Utiliza��o do algoritmo k-NN para classifica��o preditiva em dados de diagn�stico de diabetes
### Autor: Ricardo Avancini
### e-mail: avancini.rf@gmail.com
### Reposit�rio: https://github.com/avancinirf/rknn
### ---------------------------------------------------------------------------

# Weka DataSet
# http://www.technologyforge.net/Datasets/

# Verificando e instalando um �nico pacote
if (!require("class")) install.packages("class")
require('class')

# Selecionando o diretorio raiz para o script
setwd("C:/Users/Ricardo/Documents/SITE_POSTS/KNN/rknn")
#getwd()

# Carregando arquivo CSV com os dados de diagn�stico de diabetes
dados <- as.data.frame(read.table("data/DiabetesDiagnosis.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)) 

# Criando as vari�veis para executar a fun��o K-NN sem normaliza��o nos dados
treinamento<-dados[1:708, 1:8]
rotulo<-dados[1:708, 9:9]
teste<-dados[709:768, 1:8]
# Exemplo de como executar o algoritmo k-NN com k =5
estimado <- as.data.frame(knn(treinamento, teste, rotulo, 5))

# Cria��o de um data.frame para armazenar resultado
resultado <-as.data.frame(dados[709:768, 9]) # Coluna com o resultado original para validar o algoritmo
resultado[,2]<-as.data.frame(estimado[, 1]) # Coluna com o resultado da predi��o sem a normaliza��o


