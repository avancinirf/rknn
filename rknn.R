
### ---------------------------------------------------------------------------
### Título: Classificação Preditiva com algoritmo k-NN
### Descrição: Utilização do algoritmo k-NN para classificação preditiva em dados de diagnóstico de diabetes
### Autor: Ricardo Avancini
### e-mail: avancini.rf@gmail.com
### Repositório: https://github.com/avancinirf/rknn
### ---------------------------------------------------------------------------

# Weka DataSet
# http://www.technologyforge.net/Datasets/


#install.packages('class')
#require('class')



# Verificando e instalando um pacote com TryCatch
tryCatch({
  find.package('class')
}, warning = function(w) {
}, error = function(e) {
  install.packages('class')
}, finally = {
  require('class')
})

# Verificando e instalando um único pacote
if (!require("class")) install.packages("class")


# Exemplo de uso do setdiff()
a<-c("a", "b", "c","e")
b<-c("a","b","j","k")
setdiff(a, b) # o que tem no primeiro e não no segundo


# verificando e instalando multiplos pacotes
packages = c("dismo", "rgdal", "raster", "randomForest", "kernlab")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}











# Selecionando o diretorio raiz para o script
setwd("C:/Users/Ricardo/Documents/SITE_POSTS/KNN")
#getwd()

# Carregando arquivo CSV com os dados de cogumelos
original <- as.data.frame(read.table("DiabetesDiagnosis.csv", header=FALSE, sep=",", stringsAsFactors=FALSE)) 

# Criar data.framepara normalizar (sem cabeÃ§alho = 1 linha)
normalizado <- original[2:769,]

# Converter em numeric (double)
for(i in 1:8){
  normalizado[,i] <- as.numeric(normalizado[,i])  
}



# normalizado = x â€“ Min(X)) / (Max(X) â€“ Min(X)
for (j in 1:8){
  min = 0
  max = 0
  for (i in 1:nrow(normalizado)){
    if(normalizado[i,j] > max){max <- normalizado[i,j]}
    if(normalizado[i,j] < min){min <- normalizado[i,j]}
  }
  for (i in 1:nrow(normalizado)){
    normalizado[i,j]<-((normalizado[i,j]-min)/(max-min))
  }
}


# Criando as variÃ¡veis para executar a funÃ§Ã£o K-NN
# Usando dados originais, sem normalizaÃ§Ã£o
treinamento_sn<-original[2:709, 1:8]
rotulo_sn<-original[2:709, 9:9]
teste_sn<-original[710:769, 1:8]
estimado_sn <- as.data.frame(knn(treinamento_sn, teste_sn, rotulo_sn, 20)) 

# Criando as variÃ¡veis para executar a funÃ§Ã£o K-NN
# Usando dados normalizados
treinamento_n<-normalizado[1:708, 1:8]
rotulo_n<-normalizado[1:708, 9:9]
teste_n<-normalizado[709:768, 1:8]
estimado_n <- as.data.frame(knn(treinamento_n, teste_n, rotulo_n, 20)) 

# CriaÃ§Ã£o de um data.frame para armazenar resultado (ainda nÃ£o foi testado)
resultado <-as.data.frame(original[710:769, 9])
resultado[,2]<-as.data.frame(estimado_sn[, 1])
resultado[,3]<-as.data.frame(estimado_n[, 1])

total_sn<-0
total_n<-0
 
# for(i in 1:nrow(resultado)){
#   if(resultado[i,1] == resultado[i,2]){
#     total_sn<-total_sn+1
#   }
#   if(resultado[i,1] == resultado[i,3]){
#     total_n<-total_n+1
#   }
# }
# taxa_sn<-total_sn/nrow(resultado)*100
# taxa_n<-total_n/nrow(resultado)*100
#   
# print(paste0("Total de casos analisados: ",nrow(resultado)))
# print(paste0("Acertos sem normalizaÃ§Ã£o: ", total_sn, " = ", taxa_sn,"%"))
# print(paste0("Acertos sem normalizaÃ§Ã£o: ", total_n, " = ", taxa_n,"%"))

melhor_sn=0
melhor_n=0
taxa_sn=0
taxa_n=0

for(i in 1:200){
  
  estimado_sn <- as.data.frame(knn(treinamento_sn, teste_sn, rotulo_sn, i)) 
  estimado_n <- as.data.frame(knn(treinamento_n, teste_n, rotulo_n, i)) 

  treinamento_n<-normalizado[1:708, 1:8]
  rotulo_n<-normalizado[1:708, 9:9]
  teste_n<-normalizado[709:768, 1:8]
  
  # CriaÃ§Ã£o de um data.frame para armazenar resultado (ainda nÃ£o foi testado)
  resultado <-as.data.frame(original[710:769, 9])
  resultado[,2]<-as.data.frame(estimado_sn[, 1])
  resultado[,3]<-as.data.frame(estimado_n[, 1])
  
  
  total_sn=0
  total_n=0
  
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

print(paste0("K(sn) = ", melhor_sn, ", taxa: ", taxa_sn))
print(paste0("K(n) = ", melhor_n, ", taxa: ", taxa_n))




