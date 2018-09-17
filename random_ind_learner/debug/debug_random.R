library(gridExtra)
library(ggplot2)

source("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/debug/learners_window_3.R")
source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/debug/random_learner_3_v2.R")

n<-10
s<-10

n<-5
s<-5


sdev <- 1000000000
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

l1 <- one_learner(r,sdev)
l2 <- two_learner(r,sdev)
l3 <- three_learner(r,sdev)


# source("/Users/elena/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")
# two dependency learners do fine