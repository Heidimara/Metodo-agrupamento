
##Plotar VersÃ£o 2 escala log ###
library(cluster)
require(cluster)
require(ggplot2, quietly=T)
require(NbClust, quietly=T)
install.packages("ggplot")
library(ggplot2)

setwd("C:\\Users\\Computador\\Documents\\Heidi\\Disertação-Mestrado")  # mudar o diretorio

allelesub = load("alleleSub.rda")  # importar o banco de dados

#head(alleleAsub)  # mostrar apenas as seis primeiras linhas da tabela importada

A = alleleAsub
B = alleleBsub

n=1180

m=15

melhorK=rep(0,m)

limiar=0.5

myf2 = function(x,y, k, g) {
  grp=factor(g, levels = 1:3)
  plot(x, y, xlim=c(5, 13), ylim=c(-4,4), col=grp)
  title( main = paste("k = ", k))
# ggplot(data, aes(x, y, color=grp))+geom_point()
  
}
for (i in 1:m){
  
  eixoy = log2(A[i,])-log2(B[i,])
  eixox = (log2(A[i,])+log2(B[i,]))/2
  data=data.frame(eixox, eixoy)
  distancia = dist(data,method = "euclidean", diag = FALSE, upper = FALSE)
  CSM = c(0.5, 0, 0)
  
  
  for(k in 2:3){
    kmeans.result = kmeans(data, k, nstart = 10)
    grupos= kmeans.result$cluster
    S = silhouette(kmeans.result$cluster, distancia)
    resumo = summary(S)
    CSM[k] = resumo$avg.width
  }
melhorK[i]=which.max(CSM)
 
  kmeans.result = kmeans(data, melhorK[i], nstart = 10)
  grupos= kmeans.result$cluster
 myf2(eixox, eixoy,paste (i,"-",melhorK[3], CSM[melhorK[i]], sep = ";"), grupos);   
  
}

write(melhorK, file = stop("melhork"))

