library(ggplot2)
library(gridExtra)

source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")

###########################################
#check variation in exponential distribution
n <- 10
s <- 10
maxs <- NULL
means <- NULL
sums <- NULL
for(i in 1:1000){
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	sums <- c(sums, sum(r))
	maxs <- c(maxs, max(r))
	means <- c(means, mean(r))
}

p1 <- qplot(maxs) + geom_histogram() + ggtitle("n=10 s=10")
p2 <- qplot(means) + geom_histogram() + ggtitle("n=10 s=10")
p3 <- qplot(sums) + geom_histogram() + ggtitle("n=10 s=10")
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/variation_exp/r_var_n10_s10.png", height = 300, width = 900)
grid.arrange(p1,p2,p3,ncol=3)
dev.off()

n <- 10
s <- 20
maxs <- NULL
means <- NULL
sums <- NULL
for(i in 1:1000){
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	sums <- c(sums, sum(r))
	maxs <- c(maxs, max(r))
	means <- c(means, mean(r))
}

p1 <- qplot(maxs) + geom_histogram() + ggtitle("n=10 s=20")
p2 <- qplot(means) + geom_histogram() + ggtitle("n=10 s=20")
p3 <- qplot(sums) + geom_histogram() + ggtitle("n=10 s=20")
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/variation_exp/r_var_n10_s20.png", height = 300, width = 900)
grid.arrange(p1,p2,p3,ncol=3)
dev.off()

###############################################################
###########################################
# mean payoff
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(),s=integer(),n=integer())
ns <- c(5, 10, 15, 20)
ss <- c(5, 10, 15, 20)
sdevs <- c(0.1, 0.5, 1, 2, 5, 10)
for (n in ns){
	for (s in ss){
		#assign payoffs
		r <- matrix(0, ncol=n, nrow=s)
		for (i in 1:s){
			r[i,] <- rexp(n, rate=1)
			r[i,] <- round(2*(r[i,]^2))
		}
		for (sdev in sdevs){
			l1 <- one_learner(r,sdev)
			l2 <- two_learner(r,sdev)
			l3 <- three_learner(r,sdev)
			ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]]), 1:3, rep(sdev,3), rep(s,3), rep(n,3))
			payoffs <- rbind(payoffs, t(ps))
		}
	}
}

colnames(payoffs) <- c("pay", "algorithm", "sdev","s","n")
p <- ggplot(data=payoffs, aes(x=n, y=pay, col=algorithm)) + 
	geom_point()

