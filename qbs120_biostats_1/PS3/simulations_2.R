set.seed(26)

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
  plot(avg, xlab="x", ylab="density")
  lines(x_val, norm_proba, type="l", lty="dashed")
}

plotCLT(avg=u_mean[, 1], mean=0.75, var=(3/(80*n_val[1])))