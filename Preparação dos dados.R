

Tabelas=function(arquivo){   ## Função principal em que a unica entrada é o banco de dados
  library(readr)             ## carregar o pacote readr
  dados <- read_delim(arquivo," ", escape_double = FALSE, trim_ws = TRUE)  ## Importar o banco de dados
  
  ### Elaborando a primeira Tabela  ########################
  
  tabela1<-data.frame(rs=dados$`rs#`,
                      alelo1=substr(dados$alleles, 1, 1),
                      alelo2=substr(dados$alleles, 3, 3),
                      chr=as.character(dados$chrom),
                      pos=dados$pos, stringsAsFactors = FALSE)
  
  ### Elaborando a segunda Tabela  ########################
  
  library(reshape2)  #### Carregando o pacote reshape2
  tabela = melt(dados[,-(2:11)], "rs#")  ## Usando a função melt para elaborar a tabela onde as colunas são rs, sample e gtype 
  names(tabela)=c("rs", "sample", "gtype") ## Mudando o nome das colunas da tabela
  tabela$sample=as.character(tabela$sample)  ## Transformando a variavél sample em um vetor de caracteres
  return(list(snpinfo=tabela1, gtypes=tabela))  ## Retornar uma lista com as duas tabela elaborados acima
}


arq="C:\\Users\\Computador\\Downloads\\genotypes_chr10_ASW_phase3.2.txt"  ## Mostrar o diretorio onde tem o banco de dados
resultado=Tabelas(arq)    ## Fornecer os dados de entrada para executar a função Tabelas


################ Modificar os nomes das colunas e realizar o mapeamento entre observações e genótipos ########

setwd("C:\\Users\\Computador\\Documents\\Heidi\\Disertação-Mestrado")  # mudar o diretorio

allelesub = load("alleleSub.rda")  # importar o banco de dados

#head(alleleAsub)  # mostrar apenas as seis primeiras linhas dos dados importados

A = alleleAsub  ## conjunto de dados dos alelos A
B = alleleBsub  ## conjunto de dados dos alelos B

mapa=read_delim("C:/Users/Computador/Downloads/passing_cels_sample_map.txt", "\t", col_names = FALSE)  ## Importar o arquivo que contém o mapeamento para modificar os nomes das colunas.
names(mapa)=c("sample", "arquivo")  ## Mudar o nome das duas colunas do arquivo mapa

idx=match(colnames(A),mapa$arquivo)  ## Fazer a correspondência entre os nomes das colunas dos dados em A com as linhas do mapa$arquivo. 
mapa=mapa[idx,]   ### Ordenar as linhas dos dados mapa seguindo a ordem dada em index
colnames(A)=mapa$sample  ### Renomiar o nome das colunas do conjunto de dados A



idx=match(colnames(B),mapa$arquivo)  ## Fazer a correspondência entre os nomes das colunas dos dados em B com as linhas do mapa$arquivo.
mapa=mapa[idx,]   ### Ordenar as linhas dos dados mapa seguindo a ordem dada em index
colnames(B)=mapa$sample  ### Renomiar o nome das colunas do conjunto de dados B


all(colnames(A)==colnames(B))  ### Ver se o nome de todas as colunas dos dados A são iguais a dos dados B.





