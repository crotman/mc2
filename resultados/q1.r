# Gerar a análise do critério de parada para o algoritmo genético (pg. 1064).
rm(list=ls())
setwd("/Users/andrefarzat/Documents/mc2/dados")

data = read.table("data_t1.txt", header = TRUE)

print(
  kruskal.test(gd ~ config, data = data)
)


media = aggregate(data$gd, list(config=data$config), mean)
print (
  media[order(media$x),]
)

# 1. Pq ele parou? Pq não temos dados dos 200k?