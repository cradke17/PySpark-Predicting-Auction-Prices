#! /gpfs/software/R-3.6.0/lib64/R/bin/Rscript --vanilla
iter = 1
setwd('/gpfs/scratch/cradke')
betas = matrix(0,nrow= 25)
for(i in 1:10){
  file_temp = paste0('p',i,'b',iter,'.csv')
  beta_temp = read.csv(file_temp)
  beta_temp = beta_temp[,-1]
  beta_temp = as.numeric(beta_temp)
  beta_temp = as.matrix(beta_temp)
  betas = cbind(betas, beta_temp)
}
betas = betas[,-1]
beta_avg = rowMeans(betas)
output_file = paste0('b', iter,'.csv')
write.csv(beta_avg, output_file)
