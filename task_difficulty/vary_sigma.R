library(ggplot2)
library(gridExtra) 

#########################
## PENALTY DEPENDENCIES
#import learning algorithms
## CHUNK LEARNING
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_pay.R")

n <- 10
s <- 10
#
# n<-20
# s<-20

r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

sdevs <- c(0.05, 0.1, 0.5, 1, 2, 5, 10)
# payoffs <- matrix(nrow=length(sdevs), ncol = 5)
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer())
for (j in 1:length(sdevs)){
	sdev <- sdevs[j]
	l1 <- one_learner(r,sdev)
	l2 <- two_learner(r,sdev)
	l3 <- three_learner(r,sdev)
	l4 <- four_learner(r,sdev)
	l5 <- five_learner(r,sdev)
	# payoffs[j,] <- c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]])
	ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5))
	payoffs <- rbind(payoffs, t(ps))
}
colnames(payoffs) <- c("pay", "algorithm", "sdev")
payoffs$sdev <- as.factor(payoffs$sdev)

p <- ggplot(payoffs, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14))
	
	png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/chunk_learner/pay_sigmas_n10_s10_v5.png", height = 400, width = 450)
p
dev.off()

# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/chunk_learner/pay_sigmas_n20_s20_v4.png", height = 400, width = 450)
# p
# dev.off()



#####################
## WINDOW LEARNING
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")

n <- 20
s <- 10

r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

sdevs <- c(0.05, 0.1, 0.5, 1, 2, 3, 4, 5, 10)
# payoffs <- matrix(nrow=length(sdevs), ncol = 5)
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer())
for (j in 1:length(sdevs)){
	sdev <- sdevs[j]
	l1 <- one_learner(r,sdev)
	l2 <- two_learner(r,sdev)
	l3 <- three_learner(r,sdev)
	l4 <- four_learner(r,sdev)
	l5 <- five_learner(r,sdev)
	# payoffs[j,] <- c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]])
	ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5))
	payoffs <- rbind(payoffs, t(ps))
}
colnames(payoffs) <- c("pay", "algorithm", "sdev")
payoffs$sdev <- as.factor(payoffs$sdev)

p <- ggplot(payoffs, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14))
	
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigmas_window_n20_s10_v1.png", height = 400, width = 450)
p
dev.off()

########################################
####################################
# more ns and ss
#####################
## WINDOW LEARNING
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")

payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(),s=integer(),n=integer())

ns <- c(10,15,20)
ss <- c(10,15,20)

ns <- c(30,50)
ss <- c(30,50)
sdevs <- c(0.05, 0.1, 0.5, 1, 2, 3, 4, 5, 10)
for(n in ns){
	for(s in ss){
		#assign payoffs
		r <- matrix(0, ncol=n, nrow=s)
		for (i in 1:s){
			r[i,] <- rexp(n, rate=1)
			r[i,] <- round(2*(r[i,]^2))
		}
		for(sdev in sdevs){
			l1 <- one_learner(r,sdev)
			l2 <- two_learner(r,sdev)
			l3 <- three_learner(r,sdev)
			ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]]), 1:3, rep(sdev,3), rep(s,3), rep(n,3))
			payoffs <- rbind(payoffs, t(ps))
		}
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","s","n")
payoffs$sdev <- as.factor(payoffs$sdev)

##plot
plots <- list()
k=1
for(n1 in ns){
	for (s1 in ss){
		this_combo <- subset(payoffs, n==n1&s==s1)
		plots[[k]] <- ggplot(this_combo, aes(x = algorithm, y = pay, col=sdev)) + 
			geom_point() + 
			geom_line() +
			ylab("payoff")+ 
			ggtitle(paste("n=",n1," s=",s1,sep="")) + 
			theme (text = element_text(size = 8))
		k <- k+1
	}
}

# do.call(grid.arrange,plots)

ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigma_window_30_v1.pdf", do.call(grid.arrange,plots), width=26, height=20, units="cm")


p <- ggplot(payoffs, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14))
	
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigmas_window_n20_s10_v.png", height = 400, width = 450)
p
dev.off()

###########################################################
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")

payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(),s=integer(),n=integer())

ns <- c(5,10,15,20)
ss <- c(5,10,15,20)
#
# ns <- c(30,50)
# ss <- c(30,50)
sdevs <- c(0.05, 0.1, 0.5, 1, 2, 3, 4, 5, 10)
for(n in ns){
	for(s in ss){
		#assign payoffs
		r <- matrix(0, ncol=n, nrow=s)
		for (i in 1:s){
			r[i,] <- rexp(n, rate=1)
			r[i,] <- round(2*(r[i,]^2))
		}
		for(sdev in sdevs){
			l1 <- one_learner(r,sdev)
			l2 <- two_learner(r,sdev)
			l3 <- three_learner(r,sdev)
			l4 <- four_learner(r,sdev)
			l5 <- five_learner(r,sdev)
			ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5), rep(s,5), rep(n,5))
			payoffs <- rbind(payoffs, t(ps))
		}
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","s","n")
payoffs$sdev <- as.factor(payoffs$sdev)

##plot
plots <- list()
k=1
for(n1 in ns){
	for (s1 in ss){
		this_combo <- subset(payoffs, n==n1&s==s1)
		plots[[k]] <- ggplot(this_combo, aes(x = algorithm, y = pay, col=sdev)) + 
			geom_point() + 
			geom_line() +
			ylab("payoff")+ 
			ggtitle(paste("n=",n1," s=",s1,sep="")) + 
			theme (text = element_text(size = 8))
		k <- k+1
	}
}

# do.call(grid.arrange,plots)

# ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigma_window_4alg_v1.pdf", do.call(grid.arrange,plots), width=26, height=20, units="cm")
ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigma_window_5alg_v1.pdf", do.call(grid.arrange,plots), width=26, height=20, units="cm")



#########################################################
################### 
## 	CHUNK LEARNING
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_pay.R")

payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(),s=integer(),n=integer())

ns <- c(5,10,15,20)
ss <- c(5,10,15,20)
#
# ns <- c(30,50)
# ss <- c(30,50)
sdevs <- c(0.05, 0.1, 0.5, 1, 2, 3, 4, 5, 10)
for(n in ns){
	for(s in ss){
		#assign payoffs
		r <- matrix(0, ncol=n, nrow=s)
		for (i in 1:s){
			r[i,] <- rexp(n, rate=1)
			r[i,] <- round(2*(r[i,]^2))
		}
		for(sdev in sdevs){
			l1 <- one_learner(r,sdev)
			l2 <- two_learner(r,sdev)
			l3 <- three_learner(r,sdev)
			l4 <- four_learner(r,sdev)
			l5 <- five_learner(r,sdev)
			ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5), rep(s,5), rep(n,5))
			payoffs <- rbind(payoffs, t(ps))
		}
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","s","n")
payoffs$sdev <- as.factor(payoffs$sdev)

##plot
plots <- list()
k=1
for(n1 in ns){
	for (s1 in ss){
		this_combo <- subset(payoffs, n==n1&s==s1)
		plots[[k]] <- ggplot(this_combo, aes(x = algorithm, y = pay, col=sdev)) + 
			geom_point() + 
			geom_line() +
			ylab("payoff")+ 
			ggtitle(paste("n=",n1," s=",s1,sep="")) + 
			theme (text = element_text(size = 8))
		k <- k+1
	}
}

# do.call(grid.arrange,plots)

# ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigma_window_4alg_v1.pdf", do.call(grid.arrange,plots), width=26, height=20, units="cm")
ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigma_chunk_5alg_v1.pdf", do.call(grid.arrange,plots), width=26, height=20, units="cm")



###################################################################################
################################################################################
######### BONUS DEPENDENCIES
# LEARNING ALGORITHM

###########################################################
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_bonus_window.R")

payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(),s=integer(),n=integer())

ns <- c(5, 10)
ss <- c(5, 10)
#
# ns <- c(30,50)
# ss <- c(30,50)
sdevs <- c(0.005, 0.05, 0.1, 0.5, 1, 5, 10)
for(n in ns){
	for(s in ss){
		#assign payoffs
		r <- matrix(0, ncol=n, nrow=s)
		for (i in 1:s){
			r[i,] <- rexp(n, rate=1)
			r[i,] <- round(2*(r[i,]^2))
		}
		for(sdev in sdevs){
			l1 <- one_learner(r,sdev)
			l2 <- two_learner(r,sdev)
			l3 <- three_learner(r,sdev)
			l4 <- four_learner(r,sdev)
			l5 <- five_learner(r,sdev)
			ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5), rep(s,5), rep(n,5))
			payoffs <- rbind(payoffs, t(ps))
		}
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","s","n")
payoffs$sdev <- as.factor(payoffs$sdev)

##plot
plots <- list()
k=1
for(n1 in ns){
	for (s1 in ss){
		this_combo <- subset(payoffs, n==n1&s==s1)
		plots[[k]] <- ggplot(this_combo, aes(x = algorithm, y = pay, col=sdev)) + 
			geom_point() + 
			geom_line() +
			ylab("payoff")+ 
			ggtitle(paste("n=",n1," s=",s1,sep="")) + 
			theme (text = element_text(size = 8))
		k <- k+1
	}
}

# do.call(grid.arrange,plots)

ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/pay_sigma_window_4alg_v1.pdf", do.call(grid.arrange,plots), width=26, height=20, units="cm")
