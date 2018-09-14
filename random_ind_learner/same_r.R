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
      
#different sdev - same algorithm
source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/random_learner_3_v2.R")

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


# png("/Users/km102/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/samer_algs.png", width=300, height=700)
png("/Users/km102/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/samer_algs_v2.png", width=300, height=700)
do.call(grid.arrange,plots)
dev.off()


# png("/Users/km102/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/samer_payoffs.png", width=300, height=700)
png("/Users/km102/Google Drive/ASU/mesoudi_model/random_ind_learner/pics/dep3/samer_payoffs_v2.png", width=300, height=700)
do.call(grid.arrange,payplots)
dev.off()

###############################################
###########################
#repeats using the same reward matrix

source("~/Google Drive/ASU/mesoudi_model/random_ind_learner/random_learner_3_v2.R")
## reward matrix
n <- 10
s <- 10
# sdev <- 1
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

##repeats
sdevs <- c(0.1, 1, 5)
repeats <- 2
  
payoffs <- data.frame(pay=double(), algorithm=integer(), sdev=integer(), n=integer(), s= integer(), rep=integer())   
repertoires <- NULL
for (reps in 1:repeats){
    sdev <- sdevs[i]
	for (j in 1:length(sdevs)){
		sdev <- sdevs[j]
		l1 <- one_learner(r,sdev)
		l2 <- two_learner(r,sdev)
		l3 <- three_learner(r,sdev)
		ps <- rbind(c(l1[[2]], l2[[2]], l3[[2]]), 1:3, rep(sdev,3),rep(n,3),rep(s,3), rep(reps,3))
		payoffs <- rbind(payoffs, t(ps))
        rs1 <- rbind(l1[[1]],c(1:s),rep(1,s),rep(sdev,s),rep(n,s),rep(s,s),rep(reps,s))
        rs2 <- rbind(l2[[1]],c(1:s),rep(2,s),rep(sdev,s),rep(n,s),rep(s,s),rep(reps,s))
        rs3 <- rbind(l1[[1]],c(1:s),rep(3,s),rep(sdev,s),rep(n,s),rep(s,s),rep(reps,s))
        rss <- cbind(rs1,rs2,rs3)
        repertoires <- rbind(repertoires,t(rss))
	}
}

colnames(payoffs) <- c("pay", "algorithm", "sdev","n","s","rep")
payoffs$sdev <- as.factor(payoffs$sdev)

repertoires <- data.frame(repertoires)
colnames(repertoires) <- c("trait", "level","algorithm","sdev","n","s","rep")
repertoires$algorithm <- as.factor(repertoires$algorithm)
repertoires$rep <- as.factor(repertoires$rep)

plots <- ggplot(repertoires, aes(level, trait, col=algorithm)) +
	geom_point() + 
    # geom_jitter(height=0.1, width=0.1) +
	geom_line() +
	scale_x_continuous(breaks = pretty(data$level,n=10)) +
	scale_y_continuous(breaks = pretty(data$trait,n=10)) +
	coord_flip() +
	theme (text = element_text(size = 14)) +
	ggtitle(paste("sdev =",sdev))




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