library(ggplot2)
library(gridExtra)

source('/Users/elena/Google Drive/ASU/mesoudi_model/social_learning/individual_learning_algorithms/random_learner_updater.R')
source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/debug/learning_algorithms/random_learner_3_v3.R")

recombine <- function(nAgents,pop,pay){
	models <- sample(nAgents,2)	
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

recombine_n <-function(nModels, nAgents, pop, pay){
	#extract repertoires
	reps <- matrix(0,ncol=s,nrow=nAgents)
	for (i in 1:nAgents){
		reps[i,] <- pop[[i]]
	}
	#sample models
	models <- sample(nAgents, nModels)
	reps_models <- reps[models,]
	probs_models <- probs[models]
	#convert payoffs into copying probabilites
	probs <- rep(0,nModels)
	for(i in 1:length(models)){
		probs[i] <- pay[i] / sum(pay[models])
	}
	#for each level, copy any of the models with probability proportional to their payoff
	repertoire <- rep(0,s)
	for(level in 1:s){
		repertoire[level] <- sample(reps_models[,level], size=1, replace=T, prob=probs)
	}
}


n<-10
s<-10
# n<-20
# s<-20

r <- matrix(0, ncol=n, nrow=s)
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

# sdev<-0.001
# sdev<-0.1
sdev<-1
# sdev<-5


#INDIVIDUAL LEARNING ROUND
nAgents <- 1000
# nAgents <- 1000
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

#SOCIAL LEARNING + INDIVIDUAL LEARNING

##compare recombination and cc in one learners
nRounds <- 50
payoffs <- t(rbind(pay1, rep(1,nAgents)))
for(round in 1:nRounds){
	pay1_gen2 <- NULL
	pop1_gen2 <- NULL	
	for(agent in 1:nAgents){
		#pick two agents from the previous round
		repertoire <- recombine(nAgents,pop1,pay1)
		repertoire2 <- one_learner_update(r,sdev,repertoire)
		
		pop1_gen2 <- append(pop1_gen2,list(repertoire2[[1]]))
		pay1_gen2 <- c(pay1_gen2, calculate_payoff(s,n,r,1:n,repertoire2[[1]]))
	}
	pop1 <- pop1_gen2
	pay1 <- pay1_gen2
	payoffs <- rbind(payoffs, t(rbind(pay1,rep(round+1,nAgents))))	
}


payoffs_rec <- t(rbind(pay1, rep(1,nAgents)))
for(round in 1:nRounds){	
	pay1_rec <- NULL
	pop1_rec <- NULL
	for(agent in 1:nAgents){
		#pick two agents from the previous round
		models <- sample(nAgents,2)
		repertoire <- recombine(nAgents,pop1,pay1)

		pop1_rec <- append(pop1_rec, list(repertoire))
		pay1_rec <- c(pay1_rec, calculate_payoff(s,n,r,1:n,repertoire))
	}
	pop1 <- pop1_rec
	pay1 <- pay1_rec
	payoffs_rec <- rbind(payoffs_rec, t(rbind(pay1,rep(round+1,nAgents))))	
}

#########################################
nRounds <- 50
payoffs <- t(rbind(pay3, rep(1,nAgents)))
for(round in 1:nRounds){
	pay1_gen2 <- NULL
	pop1_gen2 <- NULL	
	for(agent in 1:nAgents){
		#pick two agents from the previous round
		repertoire <- recombine(nAgents,pop3,pay3)
		repertoire2 <- three_learner_update(r,sdev,repertoire)
		
		pop1_gen2 <- append(pop1_gen2,list(repertoire2[[1]]))
		pay1_gen2 <- c(pay1_gen2, calculate_payoff(s,n,r,1:n,repertoire2[[1]]))
	}
	pop1 <- pop1_gen2
	pay1 <- pay1_gen2
	payoffs <- rbind(payoffs, t(rbind(pay1,rep(round+1,nAgents))))	
}


payoffs_rec <- t(rbind(pay3, rep(1,nAgents)))
for(round in 1:nRounds){	
	pay1_rec <- NULL
	pop1_rec <- NULL
	for(agent in 1:nAgents){
		#pick two agents from the previous round
		models <- sample(nAgents,2)
		repertoire <- recombine(nAgents,pop3,pay3)

		pop1_rec <- append(pop1_rec, list(repertoire))
		pay1_rec <- c(pay1_rec, calculate_payoff(s,n,r,1:n,repertoire))
	}
	pop1 <- pop1_rec
	pay1 <- pay1_rec
	payoffs_rec <- rbind(payoffs_rec, t(rbind(pay1,rep(round+1,nAgents))))	
}

payoffs <- data.frame(payoffs)
# colnames(payoffs) <- c("pay1","pay2","pay3","round")
colnames(payoffs) <- c("pay1","round")
payoffs$round <- as.factor(payoffs$round)

payoffs_rec <- data.frame(payoffs_rec)
# colnames(payoffs) <- c("pay1","pay2","pay3","round")
colnames(payoffs_rec) <- c("pay1","round")
payoffs_rec$round <- as.factor(payoffs_rec$round)

plot1 <- ggplot(payoffs, aes(x=round,y=pay1)) +
	geom_boxplot()
	
plot2 <- ggplot(payoffs_rec, aes(x=round,y=pay1)) +
	geom_boxplot()

grid.arrange(plot1,plot2,ncol=1)



# nRounds <- 10
# payoffs <- t(rbind(pay1, pay2, pay3, rep(1,nAgents)))
# for(round in 1:nRounds){
# 	pay1_gen2 <- NULL
# 	pop1_gen2 <- NULL
# 	pay2_gen2 <- NULL
# 	pop2_gen2 <- NULL
# 	pay3_gen2 <- NULL
# 	pop3_gen2 <- NULL
# 	for(agent in 1:nAgents){
# 		#pick two agents from the previous round
# 		models <- sample(nAgents,2)
# 		repertoire <- recombine(models,pop1,pay1)
# 		repertoire2 <- one_learner_update(r,sdev,repertoire)
#
# 		pop1_gen2 <- append(pop1_gen2,list(repertoire))
# 		pay1_gen2 <- c(pay1_gen2, calculate_payoff(s,n,r,1:n,repertoire))
# 		## 2ND LEARNER
# 		models <- sample(nAgents,2)
# 		repertoire <- recombine(models,pop2,pay2)
# 		pop2_gen2 <- append(pop2_gen2,list(repertoire))
# 		pay2_gen2 <- c(pay2_gen2, calculate_payoff(s,n,r,1:n,repertoire))
# 		## 3RD LEARNER
# 		models <- sample(nAgents,2)
# 		repertoire <- recombine(models,pop3,pay3)
# 		pop3_gen2 <- append(pop3_gen2,list(repertoire))
# 		pay3_gen2 <- c(pay3_gen2, calculate_payoff(s,n,r,1:n,repertoire))
# 	}
# 	pop1 <- pop1_gen2
# 	pay1 <- pay1_gen2
# 	pop2 <- pop2_gen2
# 	pay2 <- pay2_gen2
# 	pop3 <- pop3_gen2
# 	pay3 <- pay3_gen2
# 	payoffs <- rbind(payoffs, t(rbind(pay1,pay2,pay3,rep(round+1,nAgents))))
# }


