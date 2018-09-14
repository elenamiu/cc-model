library(ggplot2)
library(gridExtra)
######################
## BONUS
#import learning algorithms
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_bonus_window.R")

n <- 10
s <- 10
sdev <- 5 
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

r1 <- one_learner(r,sdev)
r2 <- two_learner(r,sdev)
r3 <- three_learner(r,sdev)
r4 <- four_learner(r, sdev)
r5 <- five_learner(r,sdev)

#plot trait vs level
d1 <- cbind(c(1:s), t(r1[[1]]), rep(1,s))
d2 <- cbind(c(1:s), t(r2[[1]]), rep(2,s))
d3 <- cbind(c(1:s), t(r3[[1]]), rep(3,s))
d4 <- cbind(c(1:s), t(r4[[1]]), rep(4,s))
d5 <- cbind(c(1:s), t(r5[[1]]), rep(5,s))
data <- data.frame(rbind(d1,d2,d3,d4,d5))
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
 
 #######################################################
 ## PENALTIES
 ##
 ##
 ## Chunk learning
 #
 source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_pay.R")
 n <- 10
 s <- 10
 # sdev <- 0.1
 sdev <- 10
 r <- matrix(0, ncol=n, nrow=s)
 #assign payoffs
 for (i in 1:s){
 	r[i,] <- rexp(n, rate=1)
 	r[i,] <- round(2*(r[i,]^2))
 }

 r1 <- one_learner(r,sdev)
 r2 <- two_learner(r,sdev)
 r3 <- three_learner(r,sdev)
 r4 <- four_learner(r, sdev)
 r5 <- five_learner(r,sdev)

 #plot trait vs level
 d1 <- cbind(c(1:s), t(r1[[1]]), rep(1,s))
 d2 <- cbind(c(1:s), t(r2[[1]]), rep(2,s))
 d3 <- cbind(c(1:s), t(r3[[1]]), rep(3,s))
 d4 <- cbind(c(1:s), t(r4[[1]]), rep(4,s))
 d5 <- cbind(c(1:s), t(r5[[1]]), rep(5,s))
 data <- data.frame(rbind(d1,d2,d3,d4,d5))
 colnames(data) <- c("level","trait","algorithm")
 data$algorithm <- as.factor(data$algorithm)

 p <- ggplot(data, aes(level, trait, col=algorithm)) +
 	geom_point() + 
 	geom_line() +
 	scale_x_continuous(breaks = pretty(data$level,n=10)) +
 	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
 	coord_flip() +
	theme (text = element_text(size = 14))
	
# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_chunk_sdev01.png", width=500, height=500)
png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_chunk_sdev5.png", width=500, height=500)
p
dev.off()


## WINDOW LEARNING
 source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")
 n <- 10
 s <- 10
 sdev <- 0.1
 # sdev <- 5
 r <- matrix(0, ncol=n, nrow=s)
 #assign payoffs
 for (i in 1:s){
 	r[i,] <- rexp(n, rate=1)
 	r[i,] <- round(2*(r[i,]^2))
 }

 r1 <- one_learner(r,sdev)
 r2 <- two_learner(r,sdev)
 r3 <- three_learner(r,sdev)
 r4 <- four_learner(r, sdev)
 r5 <- five_learner(r,sdev)

 #plot trait vs level
 d1 <- cbind(c(1:s), t(r1[[1]]), rep(1,s))
 d2 <- cbind(c(1:s), t(r2[[1]]), rep(2,s))
 d3 <- cbind(c(1:s), t(r3[[1]]), rep(3,s))
 d4 <- cbind(c(1:s), t(r4[[1]]), rep(4,s))
 d5 <- cbind(c(1:s), t(r5[[1]]), rep(5,s))
 data <- data.frame(rbind(d1,d2,d3,d4,d5))
 colnames(data) <- c("level","trait","algorithm")
 data$algorithm <- as.factor(data$algorithm)

 p <- ggplot(data, aes(level, trait, col=algorithm)) +
 	geom_point() + 
	geom_jitter(height=0.1, width=0.1) + 
 	geom_line() +
 	scale_x_continuous(breaks = pretty(data$level,n=10)) +
 	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
 	coord_flip() +
	theme (text = element_text(size = 14))
	
png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_window_sdev01.png", width=500, height=500)
#png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_window_sdev5.png", width=500, height=500)
p
dev.off()



##############################################################################
######
## SAME REWARD MATRIX, DIFFERENT ALGORITHMS

n <- 10
s <- 10
# sdev <- 5
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

sdev <- 0.1
