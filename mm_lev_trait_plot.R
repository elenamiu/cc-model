library(ggplot2)
library(gridExtra)
library(iterpc)
#import learning algorithms
source("~/Google Drive/ASU/mesoudi_model/learners.R")

n <- 10
s <- 10
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

r1 <- one_learner(r,0.1)
r2 <- two_learner(r,0.1)
r5 <- five_learner(r,0.1)

#plot trait vs level
d1 <- cbind(c(1:s), t(r1), rep(1,s))
d2 <- cbind(c(1:s), t(r2), rep(2,s))
d5 <- cbind(c(1:s), t(r5), rep(5,s))
data <- data.frame(rbind(d1,d2,d5))
colnames(data) <- c("level","trait","algorithm")
data$algorithm <- as.factor(data$algorithm)

p <- ggplot(data, aes(level, trait, col=algorithm)) +
	geom_point() + 
	geom_line() +
	scale_x_continuous(breaks = pretty(data$level,n=10)) +
	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
	coord_flip()
	# scale_y_continuous(breaks = seq(1, 10, by = 1))
 #
 # tiff("/Users/elena/Google Drive/ASU/mesoudi_model/pics/traits_level.tiff")
 # p
 # dev.off()
 
n <- 20
s <- 20
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
r[i,] <- rexp(n, rate=1)
r[i,] <- round(2*(r[i,]^2))
}

r1 <- one_learner(r)
r2 <- two_learner(r)
r5 <- five_learner(r)

#plot trait vs level
d1 <- cbind(c(1:s), t(r1), rep(1,s))
d2 <- cbind(c(1:s), t(r2), rep(2,s))
d5 <- cbind(c(1:s), t(r5), rep(5,s))
data <- data.frame(rbind(d1,d2,d5))
colnames(data) <- c("level","trait","algorithm")
data$algorithm <- as.factor(data$algorithm)

p <- ggplot(data, aes(level, trait, col=algorithm)) +
	geom_point() + 
	geom_line() +
	scale_x_continuous(breaks = pretty(data$level,n=10)) +
	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
	coord_flip()
	
	
# #compare paypoffs
# a <- 1:n
# penalties<- list()
# for (level in c(2:s)){
# 	# r2 <- matrix(0, ncol=n, nrow=s-1)
# 	ps <- matrix(0,ncol=n,nrow=n)
# 	for (i in 1:n){
# 		dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
# 		penalty <- r[level,] * dep
# 		ps[i,] <- penalty
# 	}
# 	penalties[[level]] <- ps
# tp <-r[1,repertoire[1]]
# for (i in 2:s){
# 	tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
# }
# tp2 <-r[1,r2[1]]
# for (i in 2:s){
# 	tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
# }
# rp100[[run]] <- rbind(repertoire, r2)
# p100[[run]] <- c(tp, tp2)


# source("~/Google Drive/ASU/mesoudi_model/learners.R")
source("~/Google Drive/ASU/mesoudi_model/learners_window.R")


n <- 20
s <- 20
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

r1 <- one_learner(r,10)
r2 <- two_learner(r,10)
r3 <- three_learner(r,10)
r4 <- four_learner(r,10)
r5 <- five_learner(r,10)

#plot trait vs level
d1 <- cbind(c(1:s), t(r1), rep(1,s))
d2 <- cbind(c(1:s), t(r2), rep(2,s))
d3 <- cbind(c(1:s), t(r3), rep(3,s))
d4 <- cbind(c(1:s), t(r4), rep(4,s))
d5 <- cbind(c(1:s), t(r5), rep(5,s))
data <- data.frame(rbind(d1,d2,d3,d4,d5))
colnames(data) <- c("level","trait","algorithm")
data$algorithm <- as.factor(data$algorithm)


p <- ggplot(data, aes(level, trait, col=algorithm)) +
	geom_point() + 
	geom_line(position=position_dodge(width=0.3)) +
	scale_x_continuous(breaks = pretty(data$level,n=10)) +
	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
	coord_flip()
	
# ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/alg/traits_level_5_30_window.pdf", p, width=15, height=15, units="cm")
ggsave("/Users/elena/Google Drive/ASU/mesoudi_model/alg/traits_level_5_30.pdf", p, width=15, height=15, units="cm")
	
 # png("/Users/elena/Google Drive/ASU/mesoudi_model/pics/traits_level_5_20.png", height=600, width=800)
 # p
 # dev.off()
 
 #payoffs
	