# Gerar a análise de tamanho de efeito:
# comparativo de qualidade para cada instância e pares de abordagens NSGAII x MAR , NSGAII x SH e NSGAII x CPM (página 1065).

rm(list=ls())
setwd("/Users/andrefarzat/Documents/mc2/dados")
DIGIT <- 4

result <- list()
data <- read.table("data_t3-t4.txt", header = TRUE)

for (i in c('CPM', 'MAR', 'SH', 'nsga150k2x')) {
  subdata <- data[ which(data$config == i), ]
  result[[i]] <- cbind(
    best = c(round(mean(subdata$best), digits=DIGIT), round(sd(subdata$best), digits=DIGIT)),
    hv = c(round(mean(subdata$hv), digits=DIGIT), round(sd(subdata$hv), digits=DIGIT)),
    gd = c(round(mean(subdata$gd), digits=DIGIT), round(sd(subdata$gd), digits=DIGIT))
  )
  
  print(result);
}

# http://doofussoftware.blogspot.com.br/2012/07/measuring-effect-size-with-vargha.html
AMeasure <- function(a, b){
  
  # Compute the rank sum (Eqn 13)
  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])
  
  # Compute the measure (Eqn 14) 
  m = length(a)
  n = length(b)
  A = (r1/m - (m+1)/2)/n
  
  A
}

effectsize <- function (one, two) {
  effectsize_result <- cbind(
    best = each_effectsize(one[1,][1], two[2,][1]),
    hv = each_effectsize(one[1,][2], two[2,][2]),
    gd = each_effectsize(one[1,][3], two[2,][3])
    
    #best = (one[1,][1] - two[1,][1]) / one[2,][1],
    #hv = (one[1,][2] - two[1,][2]) / one[2,][2],
    #gd = (one[1,][3] - two[1,][3]) / one[2,][3]
  )

  return(effectsize_result)
}

each_effectsize <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2)))[seq_along(r1)] / m - (m + 1) / 2) / n);
}

print(effectsize(result$nsga150k2x, result$CPM))
print(effectsize(result$nsga150k2x, result$MAR))
print(effectsize(result$nsga150k2x, result$SH))

# effectsize(result$nsga150k2x, result$CPM)