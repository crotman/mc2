# Gerar a análise comparativa de qualidade para cada instância entre a NSGAII, MAR, SH, CPM (Tabela 2).

rm(list=ls())
setwd("~/Desktop/MC2/dados")

t3 = read.table("data_t3.txt", header = TRUE)
t4 = read.table("data_t4.txt", header = TRUE)