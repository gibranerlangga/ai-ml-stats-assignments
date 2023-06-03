# BiocManager::install("multtest")
library(multtest)
library(ggplot2)
data <- data(golub)

gene1.values = golub[, 1]
gene1.values[1:5]

dim(golub)
golub[, 38]

length(golub)
3051 * 38

# histogram plot of the density
hist(gene1.values)

stats4::mle()

data.frame()

hist()

sample_mean <- mean(gene1.values)
sample_sd <- sd(gene1.values)


# question 2
yeast.counts = data.frame(cells=0:12,
                          concen.1 = c(213,128,37,18,3,1,0,0,0,0,0,0,0),
                          concen.2 = c(103,143,98,42,8,4,2,0,0,0,0,0,0),
                          concen.3 = c(75,103,121,54,30,13,2,1,0,1,0,0,0),
                          concen.4 = c(0,20,43,53,86,70,54,37,18,10,5,2,2))

n = 400
cells = 0:12

get_lambda <- function(cells, concen, n) {
  return(sum(cells*concen)/n)
}

lambda_concen = c()

lambda_concen[1] <- get_lambda(cells, yeast.counts$concen.1, n)
lambda_concen[2] <- get_lambda(cells, yeast.counts$concen.2, n) 
lambda_concen[3] <- get_lambda(cells, yeast.counts$concen.3, n) 
lambda_concen[4] <- get_lambda(cells, yeast.counts$concen.4, n)

cat(paste("lambda value for concentration 1 is", lambda_concen[1], "\n",
          "lambda value for concentration 2 is", lambda_concen[2], "\n",
          "lambda value for concentration 3 is", lambda_concen[3], "\n",
          "lambda value for concentration 4 is", lambda_concen[4], "\n"
))

# d
get_ci_upper <- function(lambda, z, n) {
  return(lambda + z*sqrt(lambda/n))
}

get_ci_lower <- function(lambda, z, n) {
  return(lambda - z*sqrt(lambda/n))
}

concen_ci_upper <- c()
concen_ci_lower <- c()

# concen 1
concen_ci_lower[1] <- get_ci_lower(lambda_concen[1], 1.96, n)
concen_ci_upper[1] <- get_ci_upper(lambda_concen[1], 1.96, n)

# concen 2
concen_ci_lower[2] <- get_ci_lower(lambda_concen[2], 1.96, n)
concen_ci_upper[2] <- get_ci_upper(lambda_concen[2], 1.96, n)

# concen 3
concen_ci_lower[3] <- get_ci_lower(lambda_concen[3], 1.96, n)
concen_ci_upper[3] <- get_ci_upper(lambda_concen[3], 1.96, n)

# concen 4
concen_ci_lower[4] <- get_ci_lower(lambda_concen[4], 1.96, n)
concen_ci_upper[4] <- get_ci_upper(lambda_concen[4], 1.96, n)


cat(paste("95% confidence interval for lambda 1 are", concen_ci_lower[1], "and", concen_ci_upper[1], "\n",
          "95% confidence interval for lambda 1 are", concen_ci_lower[2], "and", concen_ci_upper[2], "\n",
          "95% confidence interval for lambda 1 are", concen_ci_lower[3], "and", concen_ci_upper[3], "\n",
          "95% confidence interval for lambda 1 are", concen_ci_lower[4], "and", concen_ci_upper[4], "\n"
          ))



# e
get_expected_count <- function(lambda, k, n) {
  return(round((lambda^k / factorial(k)) * exp(-lambda) * n, digit=2))
}

get_exp_1 <- c()
get_exp_2 <- c()
get_exp_3 <- c()
get_exp_4 <- c()

for (i in cells) {
  get_exp_1 <- append(get_exp_1, get_expected_count(lambda_concen_1, i, n))
  get_exp_2 <- append(get_exp_2, get_expected_count(lambda_concen_2, i, n))
  get_exp_3 <- append(get_exp_3, get_expected_count(lambda_concen_3, i, n))
  get_exp_4 <- append(get_exp_4, get_expected_count(lambda_concen_4, i, n))
}

concen_1_observed <- yeast.counts$concen.1
concen_1_expected <- get_exp_1
concen_2_observed <- yeast.counts$concen.2
concen_2_expected <- get_exp_2
concen_3_observed <- yeast.counts$concen.3
concen_3_expected <- get_exp_3
concen_4_observed <- yeast.counts$concen.4
concen_4_expected <- get_exp_4

data.frame(concen_1_observed, 
           concen_1_expected, 
           concen_2_observed, 
           concen_2_expected,
           concen_3_observed, 
           concen_3_expected, 
           concen_4_observed, 
           concen_4_expected)
