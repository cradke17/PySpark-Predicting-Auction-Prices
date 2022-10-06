#! /gpfs/software/R-3.6.0/lib64/R/bin/Rscript --vanilla
part_num = 1 
iter = 2

file = paste0('/gpfs/projects/AMS598/Projects/project4/project4_data_part', part_num, ".csv")
u_file = paste0('gpfs/scratch/cradke/p',part_num,'u',iter-1,'.csv')
b_file = paste0('gpfs/scratch/cradke/p',part_num,'b',iter-1,'.csv')
con_file = paste0('gpfs/scratch/cradke/b',iter-1,'.csv')

df = read.csv(file)
bik = read.csv(b_file)
uk_1 = read.csv(u_file)
bk = read.csv(con_file) 

bik = as.matrix(as.numeric(bik[,-1]))
uk_1 = as.matrix(as.numeric(u_k[,-1]))
bk = as.matrix(as.numeric(bk[,-1]))
uk = uk_1 + bik - bk 

X = as.matrix(df[, -1])
Y = df$y
fit = glm.fit(X, Y, family = binomial(link = "logit"))
beta_initial = as.matrix(fit$coefficients)

rho = 0.005
negloglik = function(beta){
  -sum(-Y*log(1 + exp(-(X%*%beta))) - (1-Y)*log(1 + exp(X%*%beta))) +
    (rho/2)*sum((beta - bk + uk )^2)
}
grad_final = optim(par = beta_initial, function(x)negloglik(x), method = "BFGS")
new_coefs = grad_final$par 
setwd("/gpfs/scratch/cradke")
write.csv(new_coefs, paste0("p",part_num,"b",iter,".csv"))
write.csv(uk, paste0("p", part_num, "u", iter, ".csv"))      