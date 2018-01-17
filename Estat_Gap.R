#### Estatistica Gap ####

eixoy = log2(A[10,])-log2(B[10,])
eixox = (log2(A[10,])+log2(B[10,]))/2
data=data.frame(eixox, eixoy)

gap = clusGap(data, FUN=kmeans, nstart=20, K.max = 3, B = 10)

plot_clusgap = function(clusgap, title="Resultado do Calculo da Estat Gap"){
  require("ggplot2")
  gstab = data.frame(clusgap$Tab, k=1:nrow(clusgap$Tab))
  p = ggplot(gstab, aes(k, gap)) + geom_line() + geom_point(size=5)
  p = p + geom_errorbar(aes(ymax=gap+SE.sim, ymin=gap-SE.sim))
  p = p + ggtitle(title)
  return(p)
}

plot_clusgap(gap)
gap$Tab[,3]
plot(eixox, eixoy)

# Função para a Estatistica Gap #

setwd("C:\\Users\\Computador\\Documents\\Heidi\\Disertação-Mestrado")  # mudar o diretorio

allelesub = load("alleleSub.rda")  # importar o banco de dados

#head(alleleAsub)  # mostrar apenas as seis primeiras linhas da tabela importada

A = alleleAsub
B = alleleBsub

n=1180

m=10

melhorK=rep(0,m)


myf2 = function(x, y, k,  g) {
  par(mfrow=c(1,2))
    grp=factor(g, levels = 1:3)
    plot(x, y, xlim=c(5, 13), ylim=c(-4,4), col=grp, main = "Gráfico de dispersão")
    plot(gap, main = paste("Estat Gap;k = ", k))
    
    # ggplot(data, aes(x, y, color=grp))+geom_point()
  
}


for (i in 1:m){
  
  eixoy = log2(A[i,])-log2(B[i,])
  eixox = (log2(A[i,])+log2(B[i,]))/2
  data=data.frame(eixox, eixoy)
  gap = clusGap(data, FUN=kmeans, nstart=20, K.max = 3, B = 10)
 
  
  
for(k in 2:3){
    kmeans.result = kmeans(data, k, nstart = 10)
    grupos= kmeans.result$cluster
    estatgap=gap$Tab[,3] 
    melhorK[i]=which.max(estatgap)
  myf2(eixox, eixoy,paste (i,"-",melhorK[i], estatgap[melhorK[i]], sep = ";"), grupos);   
  
}
  
    }