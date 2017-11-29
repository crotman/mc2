# Gerar a análise comparativa de qualidade para cada instância entre a NSGAII, MAR, SH, CPM (Tabela 2).

rm(list=ls())
setwd("~/Documents/mc2/dados")

data = read.table("data_t3-t4.txt", header = TRUE)
#segregando por instancias

DIGIT = 4

instancias <- unique(data$inst)

for (instancia in instancias)
{
  print("-----------------------------------------------------")
  print(instancia)
  print("-----------------------------------------------------")
  
  mar_data = data[ which(data$config == 'MAR' & data$inst == instancia), ]
  mar = cbind(
    best = mar_data$best,
    hv = mar_data$hv,
    gd = mar_data$gd
  )
  
  sh_data = data[ which(data$config == 'SH' & data$inst == instancia), ]
  sh = cbind(
    best = sh_data$best,
    hv = sh_data$hv,
    gd = sh_data$gd
  )
  
  cpm_data = data[ which(data$config == 'CPM' & data$inst == instancia), ]
  cpm = cbind(
    best = cpm_data$best,
    hv = cpm_data$hv,
    gd = cpm_data$gd
  )
  
  nsga_data = data[ which(data$config == 'nsga150k2x' & data$inst == instancia), ]
  nsga = cbind(
    best = paste(round(mean(nsga_data$best), digits=DIGIT), "±", round(sd(nsga_data$best), digits=DIGIT)),
    hv = paste(round(mean(nsga_data$hv), digits=DIGIT), "±",round(sd(nsga_data$hv), digits=DIGIT)),
    gd = paste(round(mean(nsga_data$gd), digits=DIGIT), "±", round(sd(nsga_data$gd), digits=DIGIT))
  )
  
  tab2 <- data.frame(row.names= c('I_CV','I_HV','I_GD'),
                     NSGAII = c(summary(nsga)),
                     MAR = c(mar),
                     SH = c(sh),
                     CPM = c(cpm)
  )
  print(tab2)
}