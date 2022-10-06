#! /gpfs/software/R-3.6.0/lib64/R/bin/Rscript --vanilla
part_num = 1 
iter = 1
file = paste0('/gpfs/projects/AMS598/Projects/project4/project4_data_part', part_num, ".csv")
df = read.csv(file)

X = as.matrix(df[, -1])
Y = df$y
fit = glm.fit(X, Y, family = binomial(link = "logit"))
beta_initial = as.matrix(fit$coefficients)

u_0 = matrix(data = 0, nrow = 25, ncol = 1)
b_0 = u_0
bk = b_0
uk = u_0
    
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
          
          
          