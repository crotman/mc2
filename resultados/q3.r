# Gerar a análise comparativa de qualidade para cada instância entre a NSGAII, MAR, SH, CPM (Tabela 2).

rm(list=ls())
setwd("~/Documents/mc2/dados")

data = read.table("data_t3-t4.txt", header = TRUE)
DIGIT = 4

for (i in c(0:5)) {
  instance_name = paste('I', i, sep="")
  
  cpm_data = data[ which(data$config == 'CPM' & data$inst == instance_name), ]
  cpm = cbind(
    best = cpm_data$best,
    hv = cpm_data$hv,
    gd = cpm_data$gd
  )
  
  mar_data = data[ which(data$config == 'MAR' & data$inst == instance_name), ]
  mar = cbind(
    best = mar_data$best,
    hv = mar_data$hv,
    gd = mar_data$gd
  )
  
  sh_data = data[ which(data$config == 'SH' & data$inst == instance_name), ]
  sh = cbind(
    best = sh_data$best,
    hv = sh_data$hv,
    gd = sh_data$gd
  )
  
  nsga_data = data[ which(data$config == 'nsga150k2x' & data$inst == instance_name), ]
  nsga = cbind(
    best = paste(round(mean(nsga_data$best), digits=DIGIT), "±", round(sd(nsga_data$best), digits=DIGIT)),
    hv = paste(round(mean(nsga_data$hv), digits=DIGIT), "±",round(sd(nsga_data$hv), digits=DIGIT)),
    gd = paste(round(mean(nsga_data$gd), digits=DIGIT), "±", round(sd(nsga_data$gd), digits=DIGIT))
  )
 
  print(instance_name)
  print(summary(nsga))
}