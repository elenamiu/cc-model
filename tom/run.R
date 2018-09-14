library(ggplot2)
library(gridExtra)

source("~/learning_algorithms/learners_pay.R")
# source("~/learning_algorithms/learners_window_pay.R")
# source("~/learning_algorithms/learners_bonus_window.R")

n<-10
s<-10
## create reward matrix
r <- matrix(0, ncol=n, nrow=s)
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

sdevs <- c(0.05, 0.1, 0.5, 1, 2, 5, 10)
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer())
#for all standard deviations, call learning algorithms from attached file
for (j in 1:length(sdevs)){
	sdev <- sdevs[j]
	l1 <- one_learner(r,sdev)
	l2 <- two_learner(r,sdev)
	l3 <- three_learner(r,sdev)
	l4 <- four_learner(r,sdev)
	l5 <- five_learner(r,sdev)
	#save payoffs, algorithm number, and the value of the standard deviation
	ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5))
	#add to data frame
	payoffs <- rbind(payoffs, t(ps))
}
colnames(payoffs) <- c("pay", "algorithm", "sdev")
payoffs$sdev <- as.factor(payoffs$sdev)
#plot
p <- ggplot(payoffs, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14))

