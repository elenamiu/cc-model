library(ggplot2)
library(gridExtra)

source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/debug/learning_algorithms/random_learner_3_v3.R")
n<-10
s<-10

# n<-20
# s<-20

r <- matrix(0, ncol=n, nrow=s)
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

sdev <- 1
sdev<-0.1
sdev<-5

sdev<-0.001

recombine <- function(models,pop,pay){
	r1 <- pop[[models[1]]]
	r2 <- pop[[models[2]]]
	p1 <- pay[models[1]]
	p2 <- pay[models[2]]
	#turn payoff odds ratio to probabilites
	pLearn <- (p1/p2) / ((p1/p2)+1)
	#for each level, copy with probability proportional to payoff
	repertoire <- matrix(0, ncol=s, nrow=1)
	for(level in 1:s){
		repertoire[level] <- sample(c(r1[level],r2[level]), size=1, replace=TRUE, prob=c(pLearn,1-pLearn))
	}
	return(repertoire)
}

# one_generation <- function()

#INDIVIDUAL LEARNING ROUND
# nAgents <- 50
nAgents <- 1000
pop1<-list()
pop2<-list()
pop3<-list()
pay1<-NULL
pay2<-NULL
pay3<-NULL
for(agent in 1:nAgents){
	to_learn <- sample(s)
    l1 <- one_learner(r,sdev,to_learn)
    l2 <- two_learner(r,sdev,to_learn)
    l3 <- three_learner(r,sdev,to_learn)
    pop1 <- append(pop1,list(l1[[1]]))
    pop2 <- append(pop2,list(l2[[1]]))
    pop3 <- append(pop3, list(l3[[1]]))
	pay1 <- c(pay1, l1[[2]])
	pay2 <- c(pay2, l2[[2]])
	pay3 <- c(pay3, l3[[2]])
}

## SOCIAL LEARNING
# FOR NROUNDS
# nRounds <- 10
# nRounds <- 50
nRounds <- 100
payoffs <- t(rbind(pay1, pay2, pay3, rep(1,nAgents)))
for(round in 1:nRounds){
	pay1_gen2 <- NULL
	pop1_gen2 <- NULL
	pay2_gen2 <- NULL
	pop2_gen2 <- NULL
	pay3_gen2 <- NULL
	pop3_gen2 <- NULL	
	for(agent in 1:nAgents){
		#pick two agents from the previous round
		models <- sample(nAgents,2)
		repertoire <- recombine(models,pop1,pay1)
		pop1_gen2 <- append(pop1_gen2,list(repertoire))
		pay1_gen2 <- c(pay1_gen2, calculate_payoff(s,n,r,1:n,repertoire))
		## 2ND LEARNER
		models <- sample(nAgents,2)
		repertoire <- recombine(models,pop2,pay2)
		pop2_gen2 <- append(pop2_gen2,list(repertoire))
		pay2_gen2 <- c(pay2_gen2, calculate_payoff(s,n,r,1:n,repertoire))
		## 3RD LEARNER
		models <- sample(nAgents,2)
		repertoire <- recombine(models,pop3,pay3)
		pop3_gen2 <- append(pop3_gen2,list(repertoire))
		pay3_gen2 <- c(pay3_gen2, calculate_payoff(s,n,r,1:n,repertoire))
	}
	pop1 <- pop1_gen2
	pay1 <- pay1_gen2
	pop2 <- pop2_gen2
	pay2 <- pay2_gen2
	pop3 <- pop3_gen2
	pay3 <- pay3_gen2
	payoffs <- rbind(payoffs, t(rbind(pay1,pay2,pay3,rep(round+1,nAgents))))	
}

##########################
## plot payoff distributions
payoffs <- data.frame(payoffs)
colnames(payoffs) <- c("pay1","pay2","pay3","round")
payoffs$round <- as.factor(payoffs$round)

plot1 <- ggplot(payoffs, aes(x=round,y=pay1)) +
	geom_boxplot()
	
plot2 <- ggplot(payoffs, aes(x=round,y=pay2)) +
	geom_boxplot()
	
plot3 <- ggplot(payoffs, aes(x=round,y=pay3)) +
	geom_boxplot()

# png("/Users/elena/Google Drive/ASU/mesoudi_model/social_learning/pics/payvar_n10s10_sdev1.png", height=900,width=300)
# png("/Users/elena/Google Drive/ASU/mesoudi_model/social_learning/pics/payvar_n10s10_sdev01.png", height=900,width=300)
# png("/Users/elena/Google Drive/ASU/mesoudi_model/social_learning/pics/payvar_n10s10_sdev5.png", height=900,width=300)


 # png("/Users/elena/Google Drive/ASU/mesoudi_model/social_learning/pics/payvar_n20s20_sdev001.png", height=900,width=300)
 
png("/Users/elena/Google Drive/ASU/mesoudi_model/social_learning/pics/payvar_n10s10_sdev001_1000agents_100rounds.png", height=1000,width=800)
 
grid.arrange(plot1,plot2,plot3, ncol=1)
dev.off()







# ##################################################################
# #SOCIAL LEARNING ROUND
# pop1_gen2 <- list()
# pay1_gen2 <- NULL
# for(agent in 1:nAgents){
# 	#pick two agents from the previous round
# 	models <- sample(nAgents,2)
# 	#model repertoires
# 	r1 <- pop1[[models[1]]][[1]]
# 	r2 <- pop1[[models[2]]][[1]]
# 	#model payoffs
# 	p1 <- pop1[[models[1]]][[2]]
# 	p2 <- pop1[[models[2]]][[2]]
# 	#turn payoff odds ratio to probabilites
# 	pLearn <- (p1/p2) / ((p1/p2)+1)
# 	#for each level, copy with probability proportional to payoff
# 	repertoire <- matrix(0, ncol=s, nrow=1)
# 	for(level in 1:s){
# 		repertoire[level] <- sample(c(r1[level],r2[level]), size=1, replace=TRUE, prob=c(pLearn,1-pLearn))
# 	}
# 	pop1_gen2 <- append(pop1_gen2,list(repertoire))
# 	pay1_gen2 <- c(pay1_gen2, calculate_payoff(s,n,r,1:n,repertoire))
# }
#
# #compare average payoffs over generations
 

# aux2 <- calculate_payoff(s,n,r,1:n,aux)