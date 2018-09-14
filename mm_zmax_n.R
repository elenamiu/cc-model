library(ggplot2)

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


############################################
# 10 replicates x 3 for each learning bias condition
Ns <- c(1, 3,7,10, 20,50,100,200,500,1000)

##############################################
##
## RANDOM COPYING 
##
#save population-level complexity levels for each of the 10 runs and 20 time steps

Ns <- c(1, 3,7,10)

zmax <- NULL
#run 10 replicates
for (iN in Ns){
	zrandom <- matrix(0,nrow=10, ncol=1)
	N <- iN
	print(paste("N =", N))
	for (run in c(1:10)){
	    print(paste("run ",run))
	    m <- matrix(0,nrow=N, ncol=0)
	    r <- matrix(0, nrow=n, ncol=0)
	    #SIMULATION
	    for (j in c(1:nRounds)){
	        #replace pop with N naive individuals
	        m_naive <- matrix(0, nrow=N, ncol=ncol(m))
	        #assign list of effort budgets for each individual
	        e <- rep(l,N)
	        # for every naive individual
	        for (i in c(1:N)){
	            ##random copying
	             #pick a model
	             model <- sample(c(1:N),1)
	             s <-1
				 #max s level of model
	             smax <- which(m[model,]!=0)[length(which(m[model,]!=0))]
				 #for each s-level the model knows, while the learner still has effort budget
	             while(length(smax) && e[i]>0 && s <= smax){
					 #learn that s level
	                 m_naive[i,s] <- m[model,s]
					 #cost
	                 e[i] <- e[i]-cs
	                 s<-s+1
	                 }
	            #INNOVATION  
				#while learner has effort budget left 
	            while(e[i] >0){
	                #find the first s level the individual doesn't know
	                if(any(m_naive[i,]==0)){
	                    new <- which(m_naive[i,]==0)[1] #in case other individuals know more levels than this learner
	                }else{
						#if the individual knows all the levels the population knows
	                    new <- length(m_naive[i,])+1
						#create reward distribution for new s level
	                    r <- cbind(r, rexp(n,rate=1))
	                    r[,new] <- round(2*(r[,new]^2))
						#add empty s-level to repertoire matrix
	                    m_naive <- cbind(m_naive, rep(0,N)) 
	                }
	                #pick a trait at random at level new and learn it
	                #sample n traits
	                nt <- sample(c(1:n),n)
	                #learning is costly even if unsuccessful
	                e[i] <- e[i]-ci
					#skip traits if reward is zero
	                ii <-1
	                while(r[nt[ii],new] == 0){
	                    ii <- ii+1
	                }
	                if (ii<=n){
						#learn new trait
	                    m_naive[i,new] <- nt[ii]
	                }
	            }
	        }
			#update repertoire matrix
	        m <- m_naive    
	        #figure out max population complexity
	        zpop <- NULL
			#for each individual
	        for(i in c(1:N)){
	            smax <- which(m[i,]!=0)[length(which(m[i,]!=0))]
	            zi <- 0
				#for each s-level that individual knows
	            for (s in c(1:smax)){
	                zi <- zi + r[m[i,s],s]
	            }
				#add to population array
	            zpop <- c(zpop,zi)
	        }
	    }
	    zrandom[run,] <- max(zpop)
	}
	zmax <- c(zmax, mean(zrandom))
}

plot(colMeans(zrandom[1:10,]))
# zmax
 # [1] 1263.9 1346.0 1341.2 1342.4 1308.0 1454.2 1532.9 1621.6 1676.0 1712.9



############################################
###
## INDIRECT BIAS
##

##################### fix for N=1
zmax2 <- NULL
#run 10 replicates
for (iN in Ns){
	N <- iN
	print(paste("N =", N))
	zidb <- matrix(0,nrow=10, ncol=1)
	for (run in c(1:10)){
	    print(paste("run",run))  
	    m <- matrix(0,nrow=N, ncol=0)
	    r <- matrix(0, nrow=n, ncol=0)
	    # INDIRECT BIAS
	    for (j in c(1:nRounds)){
	        # if(floor(j/3) == j/3){
        # 	            print(j)
        # 	        }
	        m_naive <- matrix(0, nrow=N, ncol=ncol(m))
	        e <- rep(l,N)
	        # for every naive individual
	        for (i in c(1:N)){
	            # #COPYING
	             #indirect bias
	             #check with individual has the highest individual fitness
	             if(ncol(m)){
	                 ifitness <- rep(0,N)
	                 for(ind in 1:n){
	                     for(s in c(1:length(m[ind,]))){
	                         ifitness[ind] <- ifitness[ind]+r[m[ind,s],s]
	                     }
	                 }
	                 if(sum(ifitness>0)){
	                     models <-which(ifitness == max(ifitness))
	                     if(length(models) == 1){
	                         model<-models
	                     } else{
	                         model <- sample(models,1)
	                     }
	                     #copy each level one by one
	                     s<-1
	                     smax <-which(m[model,]!=0)[length(which(m[model,]!=0))]
	                     while(length(smax) && e[i]>0 && s<=smax){
	                         m_naive[i,s] <- m[model,s]
	                         e[i] <- e[i]-cs
	                         s<-s+1
	                     }
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
    
	        #figure out max population complexity
	        zpop <- NULL
			#for each individual
	        for(i in c(1:N)){
	            smax <- which(m[i,]!=0)[length(which(m[i,]!=0))]
	            zi <- 0
				#for each s-level that individual knows
	            for (s in c(1:smax)){
	                zi <- zi + r[m[i,s],s]
	            }
				#add to population array
	            zpop <- c(zpop,zi)
	        }
	    }
	    zidb[run,] <- max(zpop)
	}
	zmax2 <- c(zmax2, mean(zidb))
}

############################################
###
## DIRECT BIAS
##

zdb <- matrix(0,nrow=10, ncol=20)
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
	    z3 <- c(z3,mean(zpop))
	}
    zdb[run,] <- z3
}

############################
## plot complexity levels over time; 20 timepoints, for each learning bias, averaged over 10 runs
# a<-colMeans(zrandom)
# b<-colMeans(zidb)
# c<-colMeans(zdb)
d1 <- data.frame(cbind(c(1:nRounds), colMeans(zrandom), rep(1,nRounds)))
d2 <- data.frame(cbind(c(1:nRounds), colMeans(zidb), rep(2,nRounds)))
d3 <- data.frame(cbind(c(1:nRounds), colMeans(zdb), rep(3,nRounds)))
data <- rbind(d1,d2,d3)
colnames(data)<- c("time", "complexity", "bias")
data$bias <- as.factor(data$bias)

p<- ggplot(data, aes(x=time, y=complexity, col=bias))+
	geom_line() +
	scale_color_discrete(labels=c("random","indirect","direct"))

tiff("/Users/elena/Google Drive/ASU/mesoudi_model/pics/bias.tiff", height=10, width = 15, res=300, units="cm")
p
dev.off()