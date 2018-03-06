
### ---------------------------------------------------------------------------
### Título: Classificação Preditiva com algoritmo k-NN
### Descrição: Utilização do algoritmo k-NN para classificação preditiva em dados de diagnóstico de diabetes
### realizando testes para identificar os melhores parâmetros (k e tabela com dados normalizados ou não)
### Autor: Ricardo Avancini
### e-mail: avancini.rf@gmail.com
### Repositório: https://github.com/avancinirf/rknn
### ---------------------------------------------------------------------------

# Weka DataSet
# http://www.technologyforge.net/Datasets/

# Verificando e instalando um único pacote
if (!require("class")) install.packages("class")
require('class')

# Importando a Função de Normalização (rnorm) do GitHub
source("https://raw.githubusercontent.com/avancinirf/rnorm/master/rnorm.R")

# Selecionando o diretorio raiz para o script
setwd("C:/Users/Ricardo/Documents/SITE_POSTS/KNN/rknn")
#getwd()

# Carregando arquivo CSV com os dados de diagnóstico de diabetes
original <- as.data.frame(read.table("data/DiabetesDiagnosis.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)) 

# Criar data.frame normalizado usando a função rnorm
normalizado <- rnorm("data/DiabetesDiagnosis.csv")

# Contadores para gerar a estatística dos melhores parâmetros de predição
melhor_sn=0 # Valor do K que obteve maior valor de predição sem normalização
taxa_sn=0 # Taxa de acerto (%) para o K que obteve maior valor de predição sem normalização
melhor_n=0 # Valor do K que obteve maior valor de predição com normalização
taxa_n=0 # Taxa de acerto (%) para o K que obteve maior valor de predição com normalização

# Loop para testar 200 possibilidades de valores para K, indo de 1 até 200
for(i in 1:200){

  # Gera as variáveis de treinamento, teste e os rótulos para os dados com normalização
  treinamento_n<-normalizado[1:708, 1:8]
  rotulo_n<-normalizado[1:708, 9:9]
  teste_n<-normalizado[709:768, 1:8]
  
  # Gera as variáveis de treinamento, teste e os rótulos para os dados sem normalização
  treinamento_sn<-original[1:708, 1:8]
  rotulo_sn<-original[1:708, 9:9]
  teste_sn<-original[709:768, 1:8]
  
  estimado_sn <- as.data.frame(knn(treinamento_sn, teste_sn, rotulo_sn, i)) # Executa o algoritmo k-NN com K=i sem normalização
  estimado_n <- as.data.frame(knn(treinamento_n, teste_n, rotulo_n, i)) # Executa o algoritmo k-NN com K=i com normalização
  
  # Criação de um data.frame para armazenar resultado
  resultado <-as.data.frame(original[709:768, 9])
  resultado[,2]<-as.data.frame(estimado_sn[, 1])
  resultado[,3]<-as.data.frame(estimado_n[, 1])

  # Variáveis para armazenar o total de acertos com e sem normalização
  total_sn=0
  total_n=0
  
  # Loop para conferir os resultados e armazenar o total de acertos nas variáveis
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

# Condição para verificar qual foi o K mais eficiente e em qual tabela de dados e 
# armazena o resultado em uma variável k para rodar novamente o algoritmo k-NN com o valor
# adequado de K e na tabela mais apropriada (normalizada ou não)
if(taxa_sn>=taxa_n){
  k<-melhor_sn # Parâmetro otimizado para rodar a função knn final
  parametros <- paste0("Melhor resultado 'Sem Normalização' e k=",k) # Salvando resultados dos parâmetros selecionados
  estimado <- as.data.frame(knn(treinamento_sn, teste_sn, rotulo_sn, k)) 
  
}else{
  k<-melhor_n # Parâmetro otimizado para rodar a função knn final
  parametros <- paste0("Melhor resultado 'Com Normalização' e k=",k) # Salvando resultados dos parâmetros selecionados
  estimado <- as.data.frame(knn(treinamento_n, teste_n, rotulo_n, k))  
}


# Imprime o resultado final do script
print(paste0("K(sn) = ", melhor_sn, ", taxa: ", taxa_sn))
print(paste0("K(n) = ", melhor_n, ", taxa: ", taxa_n))
print(parametros) # Imprime os parâmetros otimizados que foram usados
estimado # Lista contendo o resultado da predição para os casos de teste

