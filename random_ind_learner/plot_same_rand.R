##############################################################################
######
## SAME REWARD MATRIX, DIFFERENT ALGORITHMS

n <- 10
s <- 10
sdev <- 1
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}


#different sdev - same algorithm
source("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/random_order_learner.R")
source("/Users/elena/Google Drive/ASU/mesoudi_model/random_ind_learner/random_learner_3.R")

sdevs <- c(0.1, 1, 5)
plots <- list()
payplots <- list()
for (i in 1:length(sdevs)){
	sdev <- sdevs[i]
    r1 <- one_learner(r,sdev)
    r2 <- two_learner(r,sdev)
    r3 <- three_learner(r,sdev)

    #plot trait vs level
    d1 <- cbind(c(1:s), t(r1[[1]]), rep(1,s))
    d2 <- cbind(c(1:s), t(r2[[1]]), rep(2,s))
    d3 <- cbind(c(1:s), t(r3[[1]]), rep(3,s))
    data <- data.frame(rbind(d1,d2,d3))
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
	ps <- rbind(c(r1[[2]], r2[[2]], r3[[2]]), 1:3)
	ps <- data.frame(t(ps))
	colnames(ps) <- c("payoff", "algorithm")

	payplots[[i]] <- ggplot(ps, aes(x = algorithm, y = payoff)) + 
		geom_point() + 
		geom_line() +
		ylab("payoff")+
		theme (text = element_text(size = 14)) +
		ggtitle(paste("sdev =", sdev))
}


# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/algs_bonus_samer_sdevs.png", width=300, height=700)
do.call(grid.arrange,plots)
dev.off()


# png("/Users/elena/Google Drive/ASU/mesoudi_model/alg_explore/pics/payoffs_bonus_samer_sdevs.png", width=300, height=700)
do.call(grid.arrange,payplots)
dev.off()

