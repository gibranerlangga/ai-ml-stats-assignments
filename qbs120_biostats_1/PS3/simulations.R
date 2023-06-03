set.seed(13); 
n = 1000
t3_total = c()
for (i in 1:1000) {
  t1 = 1
  t2 = 0
  t3 = 0
  
  t2 = rpois(t1, 2)
  t3 = rpois(t2, 2)
  
  t3_total = append(t3_total, g3)
}

hist(t3_total)
mean(t3_total)
var(t3_total)

========================================================

set.seed(3)

# get value for 2nd and 3rd gen
for (i in 1:1000){
  secondgen <- rpois(i,2)
  for (j in length(secondgen)){
    thirdgen <- rpois(j, 2)
  }
}
totalpop <- c()
for (i in 1:1000){
  eachpop <- sum(secondgen[i], thirdgen[i])
  totalpop <- append(totalpop, eachpop)
}

hist(totalpop)

========================================================

set.seed(26)

# get value for 2nd and 3rd gen for 1000 diff populations
for (i in 1:1000){
  second_gen_offsp <- rpois(i,2)
  for (j in length(second_gen_offsp)){
    third_gen_offsp <- rpois(j, 2)
  }
}

# get sum value for each population from 1000 diff populations
total_p <- c()
for (i in 1:1000){
  each_p <- sum(second_gen_offsp[i], third_gen_offsp[i])
  total_p <- append(total_p, each_p)
}

hist(total_p)
mean(total_p)
var(total_p)

````````

set.seed(26)

n=5:100
mean = c()
x_val = runif(n, 0, 1)
for (i in n) {
  f_val = density(dnorm(x_val, 0, 1))
  g_val = density(runif(n, 0, 1))
  mean[i] = mean(f_val$x / g_val$x)
}

plot(mean, type="l", xlab="n", ylab="mean", title="abc")
I_g = pnorm(1) - pnorm(0)
abline(h=I_g, lty="dashed")




``````````````

set.seed(13)

n_sim = 100000
n_val = 20

u_mean = matrix(nrow = n_sim, ncol = 4)
b_mean = matrix(nrow = n_sim, ncol = 4)
u_val = c()
b_val = c()
norm_proba = c()

func = function(x) {
  return(3*x^2)
}

for (i in 1:length(n_val)) {
  n = n_val[i]
  u_val = matrix(runif(n*n_sim), nrow = n_sim, ncol = n)
  b_val = matrix(func(n*n_sim), nrow = n_sim, ncol = n)
  u_mean[, i] = apply(u_val, 1, mean)
  b_mean[, i] = apply(b_val, 1, mean)
}

plotCLT = function(avg, mean, var) {
  x_val = seq(from=0, to=1, by=0.01)
  norm_proba = dnorm(x_val, mean=mean, sd=sqrt(var))
  plot(avg, xlab="x", ylab"density")
  lines(x_val, norm_proba, type="l", lty="dashed")
}

plotCLT(avg = u_mean[, 1], mean = 0.75, val = 3/(80*n_val[1]))


-------------------------------------------------------
  
  #my data
set.seed(26)
x<-rnorm(10,50,4)
y<-rnorm(10,50,7)
df<-data.frame(x,y) 

PCA based on covariance matrix and on Correlation matrix
PCA_df.cov <- prcomp(df, scale=F)
PCA_df.corr <- prcomp(df, scale=T, center=T)

PCA_df.cov
PCA_df.corr
