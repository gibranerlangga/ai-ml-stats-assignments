choose(10, 9)

(0.5)^10 * 22

get_theta <- function(n, k, p) {
  return(round(choose(n, k) * p^k * (1-p)^(n-k), digits=7))
}

theta_0 <- get_theta(5, 0, 0.142)
theta_1 <- get_theta(5, 1, 0.142)
theta_2 <- get_theta(5, 2, 0.142)
theta_3 <- get_theta(5, 3, 0.142)
theta_4 <- get_theta(5, 4, 0.142)
theta_5 <- get_theta(5, 5, 0.142)

p = 0.142
n = 5
k = 0:5

theta = c()

for (i in k) {
  print(i)
  print(n)
  print(p)
  theta[i] = get_theta(n, i, p)
  print(theta[i])
}

for (i in k) {
  print(i)
}

for (res in theta) {
  print(res)
}


bar <- c(0, 1, 2, 3, 4, 5)
freq <- c(157, 69, 35, 17, 1, 1)


get_pearson <- function(a, b) {
  return((a-b)^2 / b)
}

res <- c()

for (i in 0:5) {
  temp = get_pearson(bar[i], freq[i])
  res <- append(res, temp)
}

res
bar

var.theta.hat <- (0.768*(1-.768))/(2*190)

get_theta <- function(n, k, p) {
  return(round(choose(n, k) * p^k * (1-p)^(n-k), digits=7))
}

theta_0 <- get_theta(5, 0, 0.142)
theta_1 <- get_theta(5, 1, 0.142)
theta_2 <- get_theta(5, 2, 0.142)
theta_3 <- get_theta(5, 3, 0.142)
theta_4 <- get_theta(5, 4, 0.142)
theta_5 <- get_theta(5, 5, 0.142)

cat(paste("theta value for k = 0 is", theta_0, "\n",
          "theta value for k = 1 is", theta_1, "\n",
          "theta value for k = 2 is", theta_2, "\n",
          "theta value for k = 3 is", theta_3, "\n",
          "theta value for k = 4 is", theta_4, "\n",
          "theta value for k = 5 is", theta_5, "\n"
))

get_pearson <- function(a, b) {
  return((a-b)^2 / b)
}

bar <- c(0, 1, 2, 3, 4, 5)
freq <- c(157, 69, 35, 17, 1, 1)
e <- 280 * c(theta_0, theta_1, theta_2, theta_3, theta_4, theta_5)

pearson_val <- c()

for (i in 1:length(bar)) {
  temp = get_pearson(freq[i], e[i])
  cat(paste("chi val", i, "is ", temp, "\n"))
  pearson_val <- append(pearson_val, temp)
}

get_pearson(freq[0], e[0])
e[1]
