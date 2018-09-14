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

N=10
n=10
l=100
ci=2
cs=1
copy=2

##################
# N individuals, each with si levels
# val m[i,j] = trait value that agent i has for complexity level s
m <- matrix(0,nrow=N, ncol=0)

#reward matrix
# r[i,j] = reward for trait i at level j
r <- matrix(0, nrow=n, ncol=0)
#let's populate the first row
# #   data from exponential distribution, square, double, and round
# r[,1] <- rexp(n,rate=1)
# r[,1] <- round(2*(r[,1]^2))

z <- NULL
#SIMULATION
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
        if (copy==1){
            ##
         #random copying
         #pick a model
         model <- sample(c(1:N),1)
         s <-1
         smax <- which(m[model,]!=0)[length(which(m[model,]!=0))]
         while(length(smax) && e[i]>0 && s <= smax){
             m_naive[i,s] <- m[model,s]
             e[i] <- e[i]-cs
             s<-s+1
             }
         } else if(copy==2){
             ##
             #indirect bias
             #check with individual has the highest individual fitness
             if(ncol(m)){
                 ifitness <- rep(0,N)
                 for(ind in c(1:n)){
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
         }else if(copy==3){
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
         } else {
             print("Invalid copying mechanism - please choose between \n1 - random copying\n2 - indirect bias\3 - direct bias")}
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
