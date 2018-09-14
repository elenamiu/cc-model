##############################################################################
######
## SAME REWARD MATRIX, DIFFERENT ALGORITHMS

n <- 10
s <- 10
# sdev <- 1
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}
      
# 	  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,]    1   12   56    0    9    1    1    1    0     2
#  [2,]    4    0    0    0    7    9    1    4    8     1
#  [3,]    0   21    0    0    8    0    0    1   15     1
#  [4,]    0   23    1    3    8    0    3    3    0     0
#  [5,]    1    0    0    0   72    2   10    0    3     2
#  [6,]    0    5    1    6   16    0    0    0    0     1
#  [7,]    1    2    3    0    1    1    0    6    2     1
#  [8,]    0    5    1    0    1    0   21    0    1     1
#  [9,]    4    2   10   12    0    0    0    3    0     9
# [10,]    0    1    1    0    3    0    1    3    0     4

# > r
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,]    0    3    5    0    2    1    0    0    0    15
#  [2,]    3   83    0    6    6    3    4    1    5     0
#  [3,]    2    0    3    0    0    0    2    1    2     1
#  [4,]    5    0    0    0   16    3    3    0    0     0
#  [5,]    0    5    7    0    7    2    6    0    4     0
#  [6,]    0    3   50    6    0    1    0    0    2     0
#  [7,]    0    0    0   11    0    1   10    1    1     5
#  [8,]    0    1    0   21    0    4    1    3    0     4
#  [9,]   24    0    2    1    1   10    0    1    0     0
# [10,]    0    5    1    0    0    0    4   13    0     8


#different sdev - same algorithm
# source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_window_pay.R")
source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_pay.R")
# source("~/Google Drive/ASU/mesoudi_model/learning_algorithms/learners_bonus_window.R")

sdevs <- c(0.1, 1, 5)
plots <- list()
payplots <- list()
for (i in 1:length(sdevs)){
	sdev <- sdevs[i]
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

    plots[[i]] <- ggplot(data, aes(level, trait, col=algorithm)) +
    	geom_point() + 
   	geom_jitter(height=0.1, width=0.1) + 
    	geom_line() +
    	scale_x_continuous(breaks = pretty(data$level,n=10)) +
    	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
    	coord_flip() +
   		theme (text = element_text(size = 14)) +
		ggtitle(paste("sdev =",sdev))
	
	#plot payoffs for each sdev
	ps <- rbind(c(r1[[2]], r2[[2]], r3[[2]], r4[[2]], r5[[2]]), 1:5)
	ps <- data.frame(t(ps))
	colnames(ps) <- c("payoff", "algorithm")

	payplots[[i]] <- ggplot(ps, aes(x = algorithm, y = payoff)) + 
		geom_point() + 
		geom_line() +
		ylab("payoff")+
		theme (text = element_text(size = 14)) +
		ggtitle(paste("sdev =", sdev))
}

# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_window_samer_sdevs.png", width=300, height=700)
png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_chunk_samer_sdevs.png", width=300, height=700)
# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_bonus_samer_sdevs.png", width=300, height=700)
do.call(grid.arrange,plots)
dev.off()

# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/payoffs_window_samer_sdevs.png", width=300, height=700)
png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/payoffs_chunk_samer_sdevs.png", width=300, height=700)
# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/payoffs_bonus_samer_sdevs.png", width=300, height=700)
do.call(grid.arrange,payplots)
dev.off()

