##4/12/18

# N <- 100
<<<<<<< Local Changes
N <- 1000
=======
>>>>>>> External Changes
n <- 100
ci <- 10
cs <- 5
l <- 1000
mi <- 0
ms <- 0
nRounds <- 20
copy <- 3


################################
# 10 replicates x 3 for three different population sizes

z10 <- matrix(0,nrow=10, ncol=20)
for (run in c(1:10)){
    print(paste("run",run))
    z <- NULL    
    m <- matrix(0,nrow=N, ncol=0)
    r <- matrix(0, nrow=n, ncol=0)
	N <- 10
	#SIMULATION
	##
	# DIRECT BIAS
	for (j in c(1:nRounds)){
	    if(floor(j/3) == j/3){
	        print(j)
	    }
	    #replace pop with N naive individuals
	    m_naive <- matrix(0, nrow=N, ncol=ncol(m))
	    #assign list of effort budgets for each individual
	    e <- rep(l,N)
	    # for every naive individual
	    for (i in c(1:N)){
	        # #COPYING
	        ##
	             # direct bias
	             if(ncol(m)){
	                 s<-1
	                 smax <- ncol(m)
	                 while(e[i]>0 && s <=smax){
	                     #traits that the population knows
	                     spop <- m[,s]
	                     #highest payoff
	                     i_trait <- which(r[spop,s]==max(r[spop,s]))
	                     if(length(i_trait) ==1){
	                         m_naive[i,s] <- spop[i_trait]
	                     } else{
	                         m_naive[i,s] <- spop[sample(i_trait,1)]
	                     }
	                     e[i] <- e[i]-cs
	                     s<-s+1   
	                 }
	             } 
	        #INNOVATION  
	        # find first s-level not known by the individual
	        while(e[i] >0){
	            #find the first s level the individual doesn't know
	            if(any(m_naive[i,]==0)){
	                new <- which(m_naive[i,]==0)[1]
	            }else{
	                new <- length(m_naive[i,])+1
	                r <- cbind(r, rexp(n,rate=1))
	                r[,new] <- round(2*(r[,new]^2))
	                m_naive <- cbind(m_naive, rep(0,N)) 
	            }
	            #pick a trait at random at level new and learn it
	            #sample n traits
	            nt <- sample(c(1:n),n)
	            #learning is costly even if unsuccessful
	            e[i] <- e[i]-ci
	            ii <-1
	            #skip is reward is zero
	            while(r[nt[ii],new] == 0){
	                ii <- ii+1
	            }
	            if (ii<=n){
	                m_naive[i,new] <- nt[ii]
	            }
	        }
	    }
	    m <- m_naive    
    
	    #figure out average population complexity at each time round
	    #   mean individual fitness
	    zpop <- NULL
	    for(i in c(1:N)){
	        smax <- which(m[i,]!=0)[length(which(m[i,]!=0))]
	        zi <- 0
	        for (s in c(1:smax)){
	            zi <- zi + r[m[i,s],s]
	        }
	        zpop <- c(zpop,zi)
	    }
	    z <- c(z,mean(zpop))
	}
    z10[run,] <- z
}


###################
z100 <- matrix(0,nrow=10, ncol=20)
for (run in c(1:10)){
    print(paste("run",run))
    z <- NULL    
    m <- matrix(0,nrow=N, ncol=0)
    r <- matrix(0, nrow=n, ncol=0)
	N <- 100
	#SIMULATION
	##
	# DIRECT BIAS
	for (j in c(1:nRounds)){
	    if(floor(j/3) == j/3){
	        print(j)
	    }
	    #replace pop with N naive individuals
	    m_naive <- matrix(0, nrow=N, ncol=ncol(m))
	    #assign list of effort budgets for each individual
	    e <- rep(l,N)
	    # for every naive individual
	    for (i in c(1:N)){
	        # #COPYING
	        ##
	             # direct bias
	             if(ncol(m)){
	                 s<-1
	                 smax <- ncol(m)
	                 while(e[i]>0 && s <=smax){
	                     #traits that the population knows
	                     spop <- m[,s]
	                     #highest payoff
	                     i_trait <- which(r[spop,s]==max(r[spop,s]))
	                     if(length(i_trait) ==1){
	                         m_naive[i,s] <- spop[i_trait]
	                     } else{
	                         m_naive[i,s] <- spop[sample(i_trait,1)]
	                     }
	                     e[i] <- e[i]-cs
	                     s<-s+1   
	                 }
	             } 
	        #INNOVATION  
	        # find first s-level not known by the individual
	        while(e[i] >0){
	            #find the first s level the individual doesn't know
	            if(any(m_naive[i,]==0)){
	                new <- which(m_naive[i,]==0)[1]
	            }else{
	                new <- length(m_naive[i,])+1
	                r <- cbind(r, rexp(n,rate=1))
	                r[,new] <- round(2*(r[,new]^2))
	                m_naive <- cbind(m_naive, rep(0,N)) 
	            }
	            #pick a trait at random at level new and learn it
	            #sample n traits
	            nt <- sample(c(1:n),n)
	            #learning is costly even if unsuccessful
	            e[i] <- e[i]-ci
	            ii <-1
	            #skip is reward is zero
	            while(r[nt[ii],new] == 0){
	                ii <- ii+1
	            }
	            if (ii<=n){
	                m_naive[i,new] <- nt[ii]
	            }
	        }
	    }
	    m <- m_naive    
    
	    #figure out average population complexity at each time round
	    #   mean individual fitness
	    zpop <- NULL
	    for(i in c(1:N)){
	        smax <- which(m[i,]!=0)[length(which(m[i,]!=0))]
	        zi <- 0
	        for (s in c(1:smax)){
	            zi <- zi + r[m[i,s],s]
	        }
	        zpop <- c(zpop,zi)
	    }
	    z <- c(z,mean(zpop))
	}
    z100[run,] <- z
}

######################
z1000 <- matrix(0,nrow=10, ncol=20)
for (run in c(1:10)){
    print(paste("run",run))
    z <- NULL    
    m <- matrix(0,nrow=N, ncol=0)
    r <- matrix(0, nrow=n, ncol=0)
	N <- 1000
	#SIMULATION
	##
	# DIRECT BIAS
	for (j in c(1:nRounds)){
	    if(floor(j/3) == j/3){
	        print(j)
	    }
	    #replace pop with N naive individuals
	    m_naive <- matrix(0, nrow=N, ncol=ncol(m))
	    #assign list of effort budgets for each individual
	    e <- rep(l,N)
	    # for every naive individual
	    for (i in c(1:N)){
	        # #COPYING
	        ##
	             # direct bias
	             if(ncol(m)){
	                 s<-1
	                 smax <- ncol(m)
	                 while(e[i]>0 && s <=smax){
	                     #traits that the population knows
	                     spop <- m[,s]
	                     #highest payoff
	                     i_trait <- which(r[spop,s]==max(r[spop,s]))
	                     if(length(i_trait) ==1){
	                         m_naive[i,s] <- spop[i_trait]
	                     } else{
	                         m_naive[i,s] <- spop[sample(i_trait,1)]
	                     }
	                     e[i] <- e[i]-cs
	                     s<-s+1   
	                 }
	             } 
	        #INNOVATION  
	        # find first s-level not known by the individual
	        while(e[i] >0){
	            #find the first s level the individual doesn't know
	            if(any(m_naive[i,]==0)){
	                new <- which(m_naive[i,]==0)[1]
	            }else{
	                new <- length(m_naive[i,])+1
	                r <- cbind(r, rexp(n,rate=1))
	                r[,new] <- round(2*(r[,new]^2))
	                m_naive <- cbind(m_naive, rep(0,N)) 
	            }
	            #pick a trait at random at level new and learn it
	            #sample n traits
	            nt <- sample(c(1:n),n)
	            #learning is costly even if unsuccessful
	            e[i] <- e[i]-ci
	            ii <-1
	            #skip is reward is zero
	            while(r[nt[ii],new] == 0){
	                ii <- ii+1
	            }
	            if (ii<=n){
	                m_naive[i,new] <- nt[ii]
	            }
	        }
	    }
	    m <- m_naive    
    
	    #figure out average population complexity at each time round
	    #   mean individual fitness
	    zpop <- NULL
	    for(i in c(1:N)){
	        smax <- which(m[i,]!=0)[length(which(m[i,]!=0))]
	        zi <- 0
	        for (s in c(1:smax)){
	            zi <- zi + r[m[i,s],s]
	        }
	        zpop <- c(zpop,zi)
	    }
	    z <- c(z,mean(zpop))
	}
    z1000[run,] <- z
}

############################
## plot complexity levels over time; 20 timepoints, for each N value
d1 <- data.frame(cbind(c(1:nRounds), colMeans(z10), rep(1,nRounds)))
d2 <- data.frame(cbind(c(1:nRounds), colMeans(z100), rep(2,nRounds)))
d3 <- data.frame(cbind(c(1:nRounds), colMeans(z1000), rep(3,nRounds)))
data <- rbind(d1,d2,d3)
colnames(data)<- c("time", "complexity", "n")
data$n <- as.factor(data$n)

p<- ggplot(data, aes(x=time, y=complexity, col=n))+
	geom_line() +
	scale_color_discrete(labels=c("10","100","1000")) +
	scale_y_continuous(breaks=seq(0,12000,1000))

tiff("/Users/elena/Google Drive/ASU/mesoudi_model/pics/popsize.tiff", height=10, width = 15, res=300, units="cm")
p
dev.off()