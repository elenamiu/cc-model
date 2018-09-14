#MESOUDI MODEL TRY1
# 4/2/2018
#N = 100, ci = 10, cs = 5, l = 1000, mi = ms = 0; 
N <- 100
n <- 100
ci <- 10
cs <- 5
l <- 1000
mi <- 0
ms <- 0
nRounds <- 20


#trial parameters

################################
# N individuals, each with n traits.
# the values m[i,j] indicates the complexity level that agent i has for trait j
m <- matrix(0, nrow=N, ncol=n)

#reward matrix
# r[i,j] indicates the reward increment for trait j at level i
#every time a new level is invented, a row is added to this matrix
r <- matrix(0,nrow=1, ncol=n)
#let's populate the firs row - draw from exponential distribution, square, double, and # round
r[1,] <- rexp(n, rate=1)
r[1,] <- round(2*(r[1,]^2))


###
z <- NULL
# SIMULATION
for (j in c(1:nRounds)){
    if (floor(j/3) == j/3){
        print(j)
    }
	#replace population with N naive individuals 
	m_naive <- matrix(0, nrow = N, ncol=n)
	#assign them a list of effort budgets e[i] - effort budget for individual i
	e <-  rep(l, N) 
	#for every naive individual
	for (i in c(1:N)){
    	#COPYING
    ####################
    	#random copying
        # #pick model
                 model <-  sample(c(1:N), 1)
                          #copy highest level of each trait
                 t <- 1
                 while(e[i] > 0 && t<=n) {
                     m_naive[i,t] <- m[model,t]
                     e[i] <- e[i]- cs
                     t <- t+1
                  }

    ########################
        # #direct bias
          # smax <- max(m)
          # s <- 1
          # while(e[i] >=0 && s <= smax){
          #     i_trait <- which(r[s,] == max(r[s,]))
          #     m_naive[i,i_trait] <- s ##??
          #     s <- s+1
          #     e[i] <- e[i] - cs
          # }
          # ##new version - copy one trait per s level
          # ss <- unique(c(m))
          # if(sum(ss) > 0){
          #     s <- 1
          #     while(e[i]>=0 && s<length(ss)){
          #         i_trait <- which(r[ss[s],] == max(r[ss[s],]))
          #         m_naive[i, i_trait] <- ss[s]
          #         s <- s+1
          #         e[i] <- e[i] - cs
          #     }
          # }
          
    # ####################
    #     #indirect bias
        #     #check which individual has the highest individual fitness
        # ifitness <-  rep(0,N)
        # for(ind in c(1:N)){
        #     for (t in c(1:n)){
        #         if(m[ind,t] > 0){
        #             ifitness[ind] <- ifitness[ind] + r[m[ind,t],t]
        #         }
        #     }
        # }
        # if(sum(ifitness > 0)){
        #     model <- which(ifitness == max(ifitness))
        #     #copy the highest level each trait one by one
        #     t <- 1
        #     while(e[i] >0 && t <=n){
        #        m_naive[i,t] <- m[model,t]
        #        e[i] <- e[i] - cs
        #        t <- t+1
        #     }
        # }

    ######################
    	#INNOVATION
	    #find first s-level not known by the individual
        while(e[i] > 0){
            new <- max(m_naive[i,]) + 1
            #add new row to reward matrix if this level hasn't been invented yet 
            if(nrow(r) < new){
                r <- rbind(r, rexp(n, rate=1))
                r[new,] <- round(2*(r[new,]^2)) 
            }
            #pick a trait at random and learn it 
            nt <- sample(c(1:n),n)
            #learning is costly even if unsuccessful
            e[i] <- e[i] - ci
            ii <- 1
            while(r[new,nt[ii]] == 0){
                ii <- ii+1
            }
            if (ii <= n){
                m_naive[i,nt[ii]] <- new        
            }
    	}
	}
	m<- m_naive
    #figure out 
    #save average population complexity at each time round
    zpop <- NULL
    for(i in c(1:N)){
        smax <- max(m[i,])
        zi <- 0
        for(s in c(1:smax)){
            slev <- m[i,] >=s
            zi <- zi + slev * r[s,]
        }
        zpop <- c(zpop, zi)
    }
    z <- c(z, mean(zpop))
}

# apply(r[ss,], 1, max)


