#13.18

(mat <- matrix(c(39,15,113,150), nrow = 2))
colnames(mat) <- c("cases","controls")
rownames(mat) <- c("exposed","not exposed")
mat

#manual calculation
mat[1,1]*mat[2,2]/(mat[2,1]*mat[1,2])

#package use
library(epitools)
(wald <- oddsratio(mat, method = "wald"))$measure

#simulation
# question 4

pi11 <- 39/(39+113) #prob of disease given exposure
pi22 <- 150/(150+15) #prob of no disease given no exposure

B <- 1000
N11 <- rbinom(B, size = 152, prob = pi11)
plot(density(N11))
N22 <- rbinom(B, size = 165, prob = pi22)
plot(density(N22))

N12 <- 152-N11
N21 <- 165-N22

OR <- (N11*N22)/(N12*N21)
plot(density(OR))
  
(sORL <- sort(OR)[25])
(sORU <- sort(OR)[975])
  
#p-value adjusting
pvals <- c(1:B)
for (i in 1:B){
  pvals[i] <- oddsratio(matrix(c(N11[i],N21[i],N12[i],N22[i]), nrow = 2),
            method = "wald")$p.value[2,2]
}

sum(pvals < .05)/1000

length(which(p.adjust(pvals, method = "bonferroni") < .05))/1000
length(which(p.adjust(pvals, method = "holm") < .05))/1000
length(which(p.adjust(pvals, method = "hochberg") < .05))/1000

length(which(p.adjust(pvals, method = "BH") < .05))/1000
length(which(p.adjust(pvals, method = "BY") < .05))/1000

qqplot(pvals, runif(B))

