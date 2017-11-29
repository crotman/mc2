# Gerar a análise do critério de parada para o algoritmo genético (pg. 1064).
rm(list=ls())
setwd("/Users/andrefarzat/Documents/mc2/dados")

data = read.table("data_t1.txt", header = TRUE)

#segregando por instancias
instancias <- unique(data$inst)

for (instancia in instancias)
{
  print("-----------------------------------------------------")
  data_inst = subset(data, data$inst == instancia)
  print (instancia)
  
  print(kruskal.test(gd ~ config, data = data_inst))
  # kruskal indica que há diferença significativa mas não onde - logo precisamos fazer o teste ad-hoc descrito
  
  print(pairwise.wilcox.test(data_inst$gd, data_inst$config, p.adj = "bonf"))
  # Daqui a gente vê que 150K apresenta diferença significativa com todos os outros
}