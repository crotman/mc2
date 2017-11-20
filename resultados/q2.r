# Gerar a análise do critério de tamanho de população do algoritmo genético (pg. 1064).

rm(list=ls())
setwd("~/Documents/mc2/dados")

data = read.table("data_t2.txt", header = TRUE)

print(
  kruskal.test(gd ~ config, data = data)
)

media = aggregate(data$gd, list(config=data$config), mean)
print (
  media[order(media$x),]
)