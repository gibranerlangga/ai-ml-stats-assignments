#### Name/Dartmouth ID: Gibran Erlangga/ F004QXJ

### Problems for Rice, Chapter 1

## Question 1
# How many unique codons are possible? How does this compare the number of unique 3 base codons for Earth DNA?
venus_bases <- c('A', 'C', 'T', 'G', 'B')
earth_bases <- c('A', 'C', 'T', 'G')

venus_codons <- 3
earth_codons <- 2



# What are the sequences of these unique 2 base codons?

# List the elements of the event E1 that both bases are the same.

# List the elements of the event E2 that both bases are different.

# List the elements of the event E3 that the first base is B.

# List the elements of the event E1 intersect E3.

# List the elements of the event E1 union E3.

## Question 2
## Question 3
## Question 4
## Question 5

### Problems for Rice, Chapter 2

## Question 1
# What is a random variable? Define both discrete and continuous RVs in terms of the sample space.
# Random variable is a function that maps sample spaces to real numbers.

get_m <- function(n, k, prob) {
  prob <- (choose(k, 1) * choose(n-k, m-1)) / choose(n,m)
           return(m)
}

expand.grid(0:1, 0:1, 0:1)

n <- 14
l <- rep(list(0:1), n)

expand.grid(l)


venus_codons <- choose(5, 2)
earth_codons <- choose(4, 3)

venus_codons
earth_codons

library(gtools)
x <- c('A', 'C', 'T', 'G', 'B')
permutations(n=5,r=2,v=x,repeats.allowed=T)
nrow(permutations(n=5,r=2,v=x,repeats.allowed=T))


venus_bases <- c('A', 'C', 'T', 'G', 'B')
earth_bases <- c('A', 'C', 'T', 'G') 

unique_venus_codons <- permutations(n=5,r=2,v=venus_bases,repeats.allowed=T)
unique_earth_codons <- permutations(n=4,r=3,v=earth_bases,repeats.allowed=T)

num_unique_venus_codons <- nrow(unique_venus_codons)
num_unique_earth_codons <- nrow(unique_earth_codons)
num_unique_venus_codons
num_unique_earth_codons

rbinom(3, 1, .5)
rbinom(1, 1, 0.5)

choose(52, 13)
