library(ggplot2)
library(tictoc)

#MESOUDI MODEL V2
# 4/10/18
N <- 100
n <- 100
ci <- 10
cs <- 5
l <- 1000
mi <- 0
ms <- 0
nRounds <- 20

#############################################
## play around with payoff distributions
library(ggplot2)
library(gridExtra)

n<-30
nreward <- rexp(n, rate=1)
nreward <- round(2*(nreward^2))

nreward2 <- rexp(n, rate=1)
nreward2 <- round(2*(nreward2^2))
plot(nreward2,type="l")

plot(dnorm(a,0,5)/dnorm(0,0,5))


#############
p1 <- qplot(c(1:length(nreward)),nreward) + geom_line()
trait1 <- 7
p2 <- qplot(c(1:length(nreward2)),nreward2) + geom_line()
trait2 <- 13
trait3 <- 27
a <- c(1:30)
increment1 <-dnorm(a,trait1,5)/dnorm(trait1,trait1,5) * mean(nreward2)
increment2 <-dnorm(a,trait1,5)/dnorm(trait1,trait1,5) * nreward[trait1]
p3 <- p2 + geom_line(aes(1:30, increment1), col="tomato")
p4 <- p2 + geom_line(aes(1:30, increment2), col="tomato")

grid.arrange(p1,p3)


######################
## 100 traits

n<-100
nreward <- rexp(n, rate=1)
nreward <- round(2*(nreward^2))

nreward2 <- rexp(n, rate=1)
nreward2 <- round(2*(nreward2^2))
#############
p1 <- qplot(c(1:length(nreward)),nreward) + geom_line()
trait1 <- 42
p2 <- qplot(c(1:length(nreward2)),nreward2) + geom_line()
a <- c(1:100)
increment1 <-dnorm(a,trait1,10)/dnorm(trait1,trait1,10) * mean(nreward2)
increment2 <-dnorm(a,trait1,10)/dnorm(trait1,trait1,10) * nreward[trait1]
p3 <- p2 + geom_line(aes(1:100, increment1), col="tomato")
p4 <- p2 + geom_line(aes(1:100, increment2), col="tomato")

grid.arrange(p1,p3,p4)

##################################################################
#######################################################


zdb <- matrix(0,nrow=10, ncol=20)
nt <-matrix(0, nrow=20)
for (run in c(1:10)){
    print(paste("run",run))
    z3 <- NULL    
    m <- matrix(0,nrow=N, ncol=0)
    r <- matrix(0, nrow=n, ncol=0)
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
					 #check - don't try to learn if level is just zeros (i.e. someone tried to innovate last round, but didn't have enough effort budget)
                     #highest payoff
					 if(sum(spop)==0){
						 break
					 }
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
		  	while(e[i] > 0){
		      #find the first s level the individual doesn't know
			new <- match(0, m_naive[i,])
			if (is.na(new)){
				new <- length(m_naive[i,])+1
				#and add another s level to the reward matrix
		          r <- cbind(r, rexp(n,rate=1))
		          r[,new] <- round(2*(r[,new]^2))
		          m_naive <- cbind(m_naive, rep(0,N)) 
				}
		      #pick a trait at random at level new and learn it
		      #sample n traits
		      nt <- sample(c(1:n),n)
		      ii <-1
		      #skip is reward is zero
		      while(r[nt[ii],new] == 0 & e[i]>0){
		          ii <- ii+1
		          #learning is costly even if unsuccessful
		          e[i] <- e[i]-ci
		      }
		      if (ii<=n & e[i]>0){
		          m_naive[i,new] <- nt[ii]
		          #learning is costly 
		          e[i] <- e[i]-ci
		      }else if(e[i]<=0){
		      	break
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
	    z3 <- c(z3,mean(zpop))
	}
    zdb[run,] <- z3
	nt[run] <- ncol(m)
}



############################################
zdep <- matrix(0,nrow=10, ncol=20)
nt_dep <- matrix(0, nrow=20)
for (run in c(1:10)){
    print(paste("run",run))
    z3 <- rep(0, nRounds)    
    m <- matrix(0,nrow=N, ncol=0)
    r <- matrix(0, nrow=n, ncol=0)
	#SIMULATION
	##
	# DIRECT BIAS
	for (j in c(1:nRounds)){
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
					 #check - don't try to learn if level is just zeros (i.e. someone tried to innovate last round, but didn't have enough effort budget)
                     #highest payoff
					 if(sum(spop)==0){
						 break
					 }
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
		  	while(e[i] > 0){
		      #find the first s level the individual doesn't know
			new <- match(0, m_naive[i,])
			if (is.na(new)){
				new <- length(m_naive[i,])+1
				#and add another s level to the reward matrix
		          r <- cbind(r, rexp(n,rate=1))
		          r[,new] <- round(2*(r[,new]^2))
		          m_naive <- cbind(m_naive, rep(0,N)) 
				}
		      #pick a trait at random at level new and learn it
		      #sample n traits
		      nt <- sample(c(1:n),n)
		      ii <-1
		      #skip is reward is zero
		      while(r[nt[ii],new] == 0 & e[i]>0){
		          ii <- ii+1
		          #learning is costly even if unsuccessful
		          e[i] <- e[i]-ci
		      }
		      if (ii<=n & e[i]>0){
		          m_naive[i,new] <- nt[ii]
		          #learning is costly 
		          e[i] <- e[i]-ci
		      }else if(e[i]<=0){
		      	break
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
	    z3[j] <- mean(zpop)
	}
    zdb[run,] <- z3
	nt[run] <- ncol(m)
}
 


