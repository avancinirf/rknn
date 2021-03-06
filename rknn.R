
### ---------------------------------------------------------------------------
### T�tulo: Classifica��o Preditiva com algoritmo k-NN
### Descri��o: Utiliza��o do algoritmo k-NN para classifica��o preditiva em dados de diagn�stico de diabetes
### realizando testes para identificar os melhores par�metros (k e tabela com dados normalizados ou n�o)
### Autor: Ricardo Avancini
### e-mail: avancini.rf@gmail.com
### Reposit�rio: https://github.com/avancinirf/rknn
### ---------------------------------------------------------------------------

# Weka DataSet
# http://www.technologyforge.net/Datasets/

# Verificando e instalando um �nico pacote
if (!require("class")) install.packages("class")
require('class')

# Importando a Fun��o de Normaliza��o (rnorm) do GitHub
source("https://raw.githubusercontent.com/avancinirf/rnorm/master/rnorm.R")

# Selecionando o diretorio raiz para o script
setwd("C:/Users/Ricardo/Documents/SITE_POSTS/KNN/rknn")
#getwd()

# Carregando arquivo CSV com os dados de diagn�stico de diabetes
original <- as.data.frame(read.table("data/DiabetesDiagnosis.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)) 

# Criar data.frame normalizado usando a fun��o rnorm
normalizado <- rnorm("data/DiabetesDiagnosis.csv")

# Contadores para gerar a estat�stica dos melhores par�metros de predi��o
melhor_sn=0 # Valor do K que obteve maior valor de predi��o sem normaliza��o
taxa_sn=0 # Taxa de acerto (%) para o K que obteve maior valor de predi��o sem normaliza��o
melhor_n=0 # Valor do K que obteve maior valor de predi��o com normaliza��o
taxa_n=0 # Taxa de acerto (%) para o K que obteve maior valor de predi��o com normaliza��o

# Loop para testar 200 possibilidades de valores para K, indo de 1 at� 200
for(i in 1:200){

  # Gera as vari�veis de treinamento, teste e os r�tulos para os dados com normaliza��o
  treinamento_n<-normalizado[1:708, 1:8]
  rotulo_n<-normalizado[1:708, 9:9]
  teste_n<-normalizado[709:768, 1:8]
  
  # Gera as vari�veis de treinamento, teste e os r�tulos para os dados sem normaliza��o
  treinamento_sn<-original[1:708, 1:8]
  rotulo_sn<-original[1:708, 9:9]
  teste_sn<-original[709:768, 1:8]
  
  estimado_sn <- as.data.frame(knn(treinamento_sn, teste_sn, rotulo_sn, i)) # Executa o algoritmo k-NN com K=i sem normaliza��o
  estimado_n <- as.data.frame(knn(treinamento_n, teste_n, rotulo_n, i)) # Executa o algoritmo k-NN com K=i com normaliza��o
  
  # Cria��o de um data.frame para armazenar resultado
  resultado <-as.data.frame(original[709:768, 9])
  resultado[,2]<-as.data.frame(estimado_sn[, 1])
  resultado[,3]<-as.data.frame(estimado_n[, 1])

  # Vari�veis para armazenar o total de acertos com e sem normaliza��o
  total_sn=0
  total_n=0
  
  # Loop para conferir os resultados e armazenar o total de acertos nas vari�veis
  for(j in 1:nrow(resultado)){
    if(resultado[j,1] == resultado[j,2]){
      total_sn=total_sn+1
    }
    if(resultado[j,1] == resultado[j,3]){
      total_n=total_n+1
    }
  }
  
  temp_sn=total_sn/nrow(resultado)*100
  temp_n=total_n/nrow(resultado)*100
  
  if(temp_sn > taxa_sn){
    taxa_sn=temp_sn
    melhor_sn=i
  }
  if(temp_n > taxa_n){
    taxa_n=temp_n
    melhor_n=i
  }
}

# Condi��o para verificar qual foi o K mais eficiente e em qual tabela de dados e 
# armazena o resultado em uma vari�vel k para rodar novamente o algoritmo k-NN com o valor
# adequado de K e na tabela mais apropriada (normalizada ou n�o)
if(taxa_sn>=taxa_n){
  k<-melhor_sn # Par�metro otimizado para rodar a fun��o knn final
  parametros <- paste0("Melhor resultado 'Sem Normaliza��o' e k=",k) # Salvando resultados dos par�metros selecionados
  estimado <- as.data.frame(knn(treinamento_sn, teste_sn, rotulo_sn, k)) 
  
}else{
  k<-melhor_n # Par�metro otimizado para rodar a fun��o knn final
  parametros <- paste0("Melhor resultado 'Com Normaliza��o' e k=",k) # Salvando resultados dos par�metros selecionados
  estimado <- as.data.frame(knn(treinamento_n, teste_n, rotulo_n, k))  
}


# Imprime o resultado final do script
print(paste0("K(sn) = ", melhor_sn, ", taxa: ", taxa_sn))
print(paste0("K(n) = ", melhor_n, ", taxa: ", taxa_n))
print(parametros) # Imprime os par�metros otimizados que foram usados
estimado # Lista contendo o resultado da predi��o para os casos de teste

