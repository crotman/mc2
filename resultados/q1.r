# Gerar a análise do critério de parada para o algoritmo genético (pg. 1064).
rm(list=ls())
setwd("/Users/andrefarzat/Documents/mc2/dados")

data = read.table("data_t1.txt", header = TRUE)

print(
  kruskal.test(gd ~ config, data = data)
)

# kruskal indica que há diferença significativa mas não onde
# logo precisamos fazer o teste ad-hoc descrito
print(
  pairwise.wilcox.test(data$gd, data$config, p.adj = "bonf")
)

# Daqui a gente vê que 150K apresenta diferença significativa com todos os outros, menos o 100K.
# Rodando a media de IGD vemos que 150K tem a menor. Logo de 100 p/ 150K o aumento não é tão significante, e 150K se torna o critério de parada
media = aggregate(data$gd, list(config=data$config), mean)
print(
  media[order(media$x),]
)