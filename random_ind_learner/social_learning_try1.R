library(ggplot2)
library(gridExtra)

source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/random_learner_3_v2.R")


# n<-10
# s<-10

n<-20
s<-20

#reward matrix
r <- matrix(0, ncol=n, nrow=s)
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

# sdev<-0.1
# sdev <- 1
# sdev<-5
sdev<-20

# nAgents <- 10
nAgents <- 1000
#first round, have 10 agents learn
pop1<-list()
pop2<-list()
pop3<-list()
for(agent in 1:nAgents){
    l1 <- one_learner(r,sdev)
    l2 <- two_learner(r,sdev)
    l3 <- three_learner(r,sdev)
    pop1 <- c(pop1,l1)
    pop2 <- c(pop2,l2)
    pop3 <- c(pop3,l3) 
}

#plot variance in payoffs in the three learning algorithms
pays <- seq(2,nAgents,2)
pay1 <- unlist(pop1[pays])
pay2 <- unlist(pop2[pays])
pay3 <- unlist(pop3[pays])

p1 <- qplot(pay1) + geom_histogram() + ggtitle(paste("one learner, sdev=",sdev)) + xlab("payoffs")
p2 <- qplot(pay2)+ geom_histogram() + ggtitle(paste("two learner, sdev=",sdev)) + xlab("payoffs")
p3 <- qplot(pay3)+ geom_histogram() + ggtitle(paste("three learner, sdev=", sdev)) + xlab("payoffs")

# png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/variation/hist_sdev01_n10s10.png", height=300,width=900)
# png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/variation/hist_sdev1_n10s10.png", height=300,width=900)
# png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/variation/hist_sdev5_n10s10.png", height=300,width=900)


png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/variation/hist_sdev20_n20s20_1000agents.png", height=300,width=900)
grid.arrange(p1,p2,p3, ncol=3)
dev.off()