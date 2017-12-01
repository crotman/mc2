# Gerar a análise de tamanho de efeito:
# comparativo de qualidade para cada instância e pares de abordagens NSGAII x MAR , NSGAII x SH e NSGAII x CPM (página 1065).

rm(list=ls())
setwd("/Users/andrefarzat/Documents/mc2/dados")
DIGIT <- 4

# http://doofussoftware.blogspot.com.br/2012/07/measuring-effect-size-with-vargha.html
AMeasure <- function(a, b){
  
  # Compute the rank sum (Eqn 13)
  r <- rank(c(a, b))
  r1 <- sum(r[seq_along(a)])
  
  # Compute the measure (Eqn 14) 
  m <- length(a)
  n <- length(b)
  A <- (r1 / m - (m + 1) / 2) / n
  A
}

data <- read.table("data_t3-t4.txt", header = TRUE)
instances <- unique(data$inst)

for (instance in instances) {
  print("-----------------------------------------------------")
  print(instance)
  print("-----------------------------------------------------")
  result <- list()
  
  mar_data = data[ which(data$config == 'MAR' & data$inst == instance), ]
  mar = cbind(
    best = mar_data$best,
    hv = mar_data$hv,
    gd = mar_data$gd
  )
  
  sh_data = data[ which(data$config == 'SH' & data$inst == instance), ]
  sh = cbind(
    best = sh_data$best,
    hv = sh_data$hv,
    gd = sh_data$gd
  )
  
  cpm_data = data[ which(data$config == 'CPM' & data$inst == instance), ]
  cpm = cbind(
    best = cpm_data$best,
    hv = cpm_data$hv,
    gd = cpm_data$gd
  )
  
  #best = paste(round(mean(nsga_data$best), digits=DIGIT), "±", round(sd(nsga_data$best), digits=DIGIT)),
  #hv = paste(round(mean(nsga_data$hv), digits=DIGIT), "±",round(sd(nsga_data$hv), digits=DIGIT)),
  #gd = paste(round(mean(nsga_data$gd), digits=DIGIT), "±", round(sd(nsga_data$gd), digits=DIGIT))
  nsga_data = data[ which(data$config == 'nsga150k2x' & data$inst == instance), ]
  nsga = cbind(best = nsga_data$best, hv = nsga_data$hv, gd = nsga_data$gd)
  print(0)
  a <- cpm_data$best
  print(a)
  print(2)
  
  
  print(AMeasure(nsga_data$best, cpm_data$best))
  #print(AMeasure(nsga, mar))
  #print(AMeasure(nsga, sh))
}




#print(AMeasure(result$nsga150k2x, result$CPM))
#print(AMeasure(result$nsga150k2x, result$MAR))
#print(AMeasure(result$nsga150k2x, result$SH))
