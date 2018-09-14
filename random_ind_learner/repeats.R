library(ggplot2)
library(gridExtra)

########################################
## INDIVIDUAL LEARNER
## RANDOM ORDER
## PENALTY DEPENDENCIES
## WINDOW LEARNING

# source("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/random_order_learner.R")
source("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/random_learner_3.R")


# n<-20
# s<-20
n<-10
s<-10

repeats <- 500
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(), n=integer(), s= integer(), rep=integer())
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
		ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]]), 1:3, rep(sdev,3),rep(n,3),rep(s,3), rep(reps,3))
		payoffs <- rbind(payoffs, t(ps))
	}
}
colnames(payoffs) <- c("pay", "algorithm", "sdev","n","s","rep")
payoffs$sdev <- as.factor(payoffs$sdev)


# write.csv(payoffs,'//Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/data_payoffs_window_n10s10.csv')


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
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/repeats_100runs_n10s10.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/repeats_100runs_n20s20.png",width=400, height=400)
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
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/repeats_100runs_n10s10_iqr.png",width=400, height=400)
png("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/repeats_100runs_n10s10_iqr.png",width=400, height=400)
p
dev.off()


################################################
##############################################
# and again with new version of three learner with no looking back
source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/random_learner_3_v2.R")

n<-20
s<-20
# n<-15
# s<-15

repeats <- 100
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(), n=integer(), s= integer(), rep=integer())
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
	    ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]]), 1:3, rep(sdev,3),rep(n,3),rep(s,3), rep(reps,3))
        payoffs <- rbind(payoffs, t(ps))
	}
}

colnames(payoffs) <- c("pay", "algorithm", "sdev","n","s","rep")
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
	

png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/v2_repeats_500runs_n10s10.png",width=400, height=400)
# png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/v2_repeats_100runs_n20s20.png",width=400, height=400)
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
	

png("~/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/v2_repeats_100runs_n10s10_iqr.png",width=400, height=400)
p
dev.off()


