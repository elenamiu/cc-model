library(ggplot2)
library(gridExtra)

########################################
## PENALTY DEPENDENCIES
## WINDOW LEARNING

source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")

# n<-5
# s<-5
# n<-10
# s<-10
n<-20
s<-20
repeats <- 100
	payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(), rep=integer())
for (reps in 1:repeats){
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	sdevs <- c(0.05, 0.1, 0.5, 1, 2, 5, 10)
	for (j in 1:length(sdevs)){
		sdev <- sdevs[j]
		l1 <- one_learner(r,sdev)
		l2 <- two_learner(r,sdev)
		l3 <- three_learner(r,sdev)
		l4 <- four_learner(r,sdev)
		l5 <- five_learner(r,sdev)
		# payoffs[j,] <- c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]])
		ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5),rep(n,5),rep(s,5), rep(reps,5))
		payoffs <- rbind(payoffs, t(ps))
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","n","s","rep")
payoffs$sdev <- as.factor(payoffs$sdev)

# write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_window.csv')
# write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_window_n10s10.csv')
write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_window_n20s20.csv')

# payoffs<- read.csv('/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_window.csv')
payoffs$sdev <- as.factor(payoffs$sdev)


#PLOT
#calculate mean 
agg<- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), mean)
#and range
agg_max <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), max)
agg_min <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), min)
agg_iqr <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), IQR)

# a <- merge(agg, agg_max, by="cauthor", all=T)
# a <- merge(a, a3, by="cauthor", all=T)

agg$min <- agg_min$pay
agg$max <- agg_max$pay
agg$iqmin <- agg$pay - agg_iqr$pay
agg$iqmax <- agg$pay + agg_iqr$pay


p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14)) +
	ggtitle("average over 100 runs")
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_window_100runs.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_window_100runs_n10s10.png",width=400, height=400)
p
dev.off()

# #range bars
# limits <- aes(ymax = max, ymin = min)
# p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) +
# 	geom_point() +
# 	# geom_line() +
# 	geom_errorbar(limits, size=0.3, alpha=0.4) +
# 	ylab("payoff")+
# 	theme (text = element_text(size = 14))
	
#iqr bars
limits <- aes(ymax = iqmax, ymin = iqmin)
p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	geom_errorbar(limits, size=0.4, alpha=0.6, width=0.3) +
	ylab("payoff")+
	theme (text = element_text(size = 14))
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_window_100runs_iqr.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_window_100runs_iqr_n10s10.png",width=400, height=400)
p
dev.off()


########################################################
######################
## 	PENALTY DEPENDENCIES
## CHUNK LEARNING
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_pay.R")
#
# n<-5
# s<-5

n<-10
s<-10
repeats <- 100
	payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(), rep=integer())
for (reps in 1:repeats){
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	sdevs <- c(0.05, 0.1, 0.5, 1, 2, 5, 10)
	for (j in 1:length(sdevs)){
		sdev <- sdevs[j]
		l1 <- one_learner(r,sdev)
		l2 <- two_learner(r,sdev)
		l3 <- three_learner(r,sdev)
		l4 <- four_learner(r,sdev)
		l5 <- five_learner(r,sdev)
		# payoffs[j,] <- c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]])
		ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5),rep(n,5),rep(s,5), rep(reps,5))
		payoffs <- rbind(payoffs, t(ps))
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","n","s","rep")
payoffs$sdev <- as.factor(payoffs$sdev)

# write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_chunk_s5n5.csv')
write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_chunk_s10n10.csv')

#PLOT
#calculate mean 
agg<- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), mean)
#and range
agg_max <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), max)
agg_min <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), min)
agg_iqr <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), IQR)

# a <- merge(agg, agg_max, by="cauthor", all=T)
# a <- merge(a, a3, by="cauthor", all=T)

agg$min <- agg_min$pay
agg$max <- agg_max$pay
agg$iqmin <- agg$pay - agg_iqr$pay
agg$iqmax <- agg$pay + agg_iqr$pay


p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14)) +
	ggtitle("average over 100 runs")
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_chunk_100runs.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_chunk_100runs_n20s20.png",width=400, height=400)
p
dev.off()

#iqr bars
limits <- aes(ymax = iqmax, ymin = iqmin)
p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	geom_errorbar(limits, size=0.4, alpha=0.6, width=0.3) +
	ylab("payoff")+
	theme (text = element_text(size = 14))
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_chunk_100runs.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_chunk_100runsiqr_n20s20.png",width=400, height=400)
p
dev.off()

##########################################################
#########################################
########################################################
######################
## 	BONUS DEPENDENCIES
## WINDOW LEARNING
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_bonus_window.R")

# n<-5
# s<-5
# n<-10
# s<-10
n<-20
s<-20
repeats <- 100
	payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(), rep=integer())
for (reps in 1:repeats){
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	sdevs <- c(0.05, 0.1, 0.5, 1, 2, 5, 10)
	for (j in 1:length(sdevs)){
		sdev <- sdevs[j]
		l1 <- one_learner(r,sdev)
		l2 <- two_learner(r,sdev)
		l3 <- three_learner(r,sdev)
		l4 <- four_learner(r,sdev)
		l5 <- five_learner(r,sdev)
		# payoffs[j,] <- c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]])
		ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]], l4[[2]], l5[[2]]), 1:5, rep(sdev,5),rep(n,5),rep(s,5), rep(reps,5))
		payoffs <- rbind(payoffs, t(ps))
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","n","s","rep")
payoffs$sdev <- as.factor(payoffs$sdev)

# write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_bonus_s5n5.csv')
# write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_bonus_s10n10.csv')
write.csv(payoffs,'/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/payoffs_bonus_s20n20.csv')

#PLOT
#calculate mean 
agg<- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), mean)
#and range
agg_max <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), max)
agg_min <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), min)
agg_iqr <- aggregate(list(pay=payoffs$pay), list(algorithm=payoffs$algorithm, sdev=payoffs$sdev, n=payoffs$n, s=payoffs$s), IQR)

# a <- merge(agg, agg_max, by="cauthor", all=T)
# a <- merge(a, a3, by="cauthor", all=T)

agg$min <- agg_min$pay
agg$max <- agg_max$pay
agg$iqmin <- agg$pay - agg_iqr$pay
agg$iqmax <- agg$pay + agg_iqr$pay


p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	ylab("payoff")+
	theme (text = element_text(size = 14)) +
	ggtitle("average over 100 runs")
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_bonus_100runs_s5n5.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_bonus_100runs_s10n10.png",width=400, height=400)
p
dev.off()

#iqr bars
limits <- aes(ymax = iqmax, ymin = iqmin)
p <- ggplot(agg, aes(x = algorithm, y = pay, col=sdev)) + 
	geom_point() + 
	geom_line() +
	geom_errorbar(limits, size=0.4, alpha=0.6, width=0.3) +
	ylab("payoff")+
	theme (text = element_text(size = 14))
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_bonus_100runs_iqr_s5n5.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/task_difficulty/pics/sigmas_bonus_100runs_iqr_s10n10.png",width=400, height=400)
p
dev.off()

