#function to calculate increments/bonuses at each level
# inc[[lev]][i,j] = increment at level lev if trait i was chosen at level lev-1 and trait j was chosen at level lev
calculate_incs <- function(s,n,r,a){
	incs<- list()
	for (level in c(2:s)){#no dependency for level 1
		ps <- matrix(0,ncol=n,nrow=n)
		#for each trait i chosen at the previous level
		for (i in 1:n){
			#define dependency window
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
            #a is just a vector 1:n that makes it easier to calculate payoffs for all potentially chosen traits in one go
			#calculate bonus for all traits in this level
            #base payoff for this level + adjusted payoff acccording to what was chosen at previous level
			inc <- r[level,] + (dep*r[level-1,i])/2
			ps[i,] <- inc
		}
        #save adjusted payoffs for this level
		incs[[level]] <- ps
	}
	return(incs)
}
####
# learning algorithms 
# consider x steps, only take one at a time
one_learner <- function(r,sdev){
    sdev <- sdev #standard deviation for dependency windo
	n <- ncol(r) #number of traits
	s <- nrow(r) #number of levels - r[i,j] - payoff of trait i at level j
	#all traits - makes it easier to calculate payoffs
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	#pick trait for first level - one with max payoff
	best <- which(r[1,] == max(r[1,]))
	#if more than one, sample from however many there are, otherwise first
	repertoire[1] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	#calculate incremented payoffs
	incs<- calculate_incs(s,n,r,a)
	#for each level lev except for the first
	for(lev in 2:s){
		#calculate dependency window
		dep <- dnorm(a,repertoire[lev-1],sdev)/dnorm(repertoire[lev-1],repertoire[lev-1],sdev)
		#bonus is payoff for current level + (dependency window x payoff for previous level)/2
		increment <- r[lev,] + (dep * r[lev-1,repertoire[lev-1]])/2
		#which trait maximises payoff
		best <- which(increment==max(increment))
		#sample if there's more than one
		repertoire[,lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	}
	#calculate total payoff
	#payoff for trait at level 1
	tp <-r[1,repertoire[1]]
	# + increments at each of the following levels, according to what's in the repertoire
	for (lev in 2:s){
		tp <- tp + incs[[lev]][repertoire[lev-1], repertoire[lev]]
	}
	#return repertoire and payoff
	ans <- list(repertoire, tp)
	return(ans)
}

two_learner <- function(r, sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
    #calculate bonuses
	incs<- calculate_incs(s,n,r,a)
    #create empty repertoire vector
	r2 <- matrix(0, ncol=s, nrow=1)
	#pick trait for first level
	#payoffs[i,j] = payoff if trait i was chosen at level 1 and trait j was chosen at level 2
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + incs[[2]][i,j]    
	    }
	}
    #which payoff is best? array index - keep only first value
	best <- which(payoffs == max(payoffs),arr.ind=T)
    #sample if there's more than one
	r2[1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1)], best[1,1])
    #for all other levels
	for(level in seq(2,s)){
        #if we're not at the last level, i.e. there's not enough levels to assess pairwise
		if(level != s){
            #payoff[i,j] = payoff if trait i were chosen at level level and trait j were chosen at level+1 
			payoffs <- matrix(0, ncol=n, nrow=n)
            #check best payoff against each trait for two consecutive levels
			for(i in 1:n){
				for(j in 1:n){
					payoffs[i,j] <-incs[[level]][r2[level-1],i] + incs[[level+1]][i,j]
				}
			}
            #which one maximises payoff 
			best <- which(payoffs == max(payoffs), arr.ind=T)
            #sample if more than one 
			r2[level] <- ifelse(nrow(best)>1,best[sample(nrow(best),1)], best[1,1])
        #if we're at the last level, do one-step learning 
		}else {
			payoffs <- incs[[level]][r2[level-1],]
			best <- which(payoffs == max(payoffs))
			r2[level] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
	}
	#calculate payoff
	tp <-r[1,r2[1]]
	for (i in 2:s){
		tp <- tp + incs[[i]][r2[i-1], r2[i]]
	}
	ans <- list(r2, tp)
	return(ans)	
}

#3-level learner
three_learner <- function(r, sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
    #calculate incremented payoffs
	incs<- calculate_incs(s,n,r,a)
	# 3-level maximiser
    #create empty repertoire
	 r3 <- matrix(0, ncol=s, nrow=1)
     #for each level
	 for (lev in seq(1,s)){
         #if there's at least 3 levels left, do 3-step learning
		 if(s-lev >=2){
             #if it's the first level, add up raw payoff at level 1 + incremented payoffs for level 2 and 3
			 if(lev==1){
                 #3-dimensional array
				 payoffs<-array(0,dim=c(n,n,n))
                 #check against each trait at each of the tree levels
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- r[lev,i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k]
				        }
				     }
				 }
                 #which one is the best
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 # #sample if there is more than one option
				 r3[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 }else{
                 #if we're not at the first level calculate payoffs for 3 successive levels
				 payoffs<-array(0,dim=c(n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- incs[[lev]][r3[lev-1],i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k]
				        }
				     }
				 }
                 #which payoff is best
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 # #sample if there is more than one option, learn first trait
				 r3[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 }
		 }
		 else if(s-lev == 1) {#if there are two levels left, do two level learning
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- incs[[lev]][r3[lev-1],i] + incs[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
            #if more than one, pick
	        r3[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r3[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
		 }
		 else if(s==lev){#if there's only one level left, do one step learning
			 best <- which(incs[[lev]][r3[lev-1],] == max(incs[[lev]][r3[lev-1],]))
			 r3[lev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
  	#payoff
  	tp <-r[1,r3[1]]
  	for (i in 2:s){
  		tp <- tp + incs[[i]][r3[i-1], r3[i]]
  	}
  	ans <- list(r3, tp)
  	return(ans)	
}

#4-level learner
four_learner <- function(r, sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	incs<- calculate_incs(s,n,r,a)
	# 4-level maximiser
    #create empty repertoire
	 r4 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s)){
		 if(s-lev >=3){#if there's at least 4 levels left, do 4-shot learning 
			 if(lev == 1){#if the first level, do raw payoff for first level + adjusted increments for the next 3
				 payoffs<-array(0,dim=c(n,n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 for(l in 1:n){
								 payoffs[i,j,k,l] <- r[lev,i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k] + incs[[lev+3]][k,l]
							 }
						 }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 #sample if there is more than one option
				 r4[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 } else {
                 #if we've passed level 2, 4-shot learner
				 payoffs<-array(0,dim=c(n,n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 for(l in 1:n){
								 payoffs[i,j,k,l] <- incs[[lev]][r4[lev-1],i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k] + incs[[lev+3]][k,l]
							 }
						 }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 #sample if there is more than one option
				 r4[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 }
		 } else if (s-lev == 2){#if there's 3 levels left, do 3-shot learning
			 payoffs<-array(0,dim=c(n,n,n))
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
						 payoffs[i,j,k] <- incs[[lev]][r4[lev-1],i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k]
			        }
			     }
			 }
			 best <- which(payoffs == max(payoffs), arr.ind=T)
			 #sample if there is more than one option
             if(nrow(best) == 1){
                 r4[lev:(lev+2)] <- best
             }else{
                 r4[lev:(lev+2)] <- best[sample(nrow(best),1),]
             }
		 } else if(s-lev == 1) {#if only two levels left, do 2-level learning
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- incs[[lev]][r4[lev-1],i] + incs[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
            #if more than one, pick
	        r4[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r4[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
		 } else if(s==lev){
			 best <- which(incs[[lev]][r4[lev-1],] == max(incs[[lev]][r4[lev-1],]))
			 r4[lev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
     #calculate payoff
  	tp <-r[1,r4[1]]
  	for (i in 2:s){
  		tp <- tp + incs[[i]][r4[i-1], r4[i]]
  	}
  	ans <- list(r4, tp)
  	return(ans)	
}


## 5-level learner
five_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	incs<- calculate_incs(s,n,r,a)
	# 5-level maximiser
    #create empty repertoire
	 r5 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s)){
         #if there's at least 5 levels left, do 5-level learning
		 if(s - lev >=4){
			 payoffs<-array(0,dim=c(n,n,n,n,n))
             #first level
			 if(lev == 1){
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
			             	for(l in 1:n){
				                 for(m in 1:n){
				                     payoffs[i,j,k,l,m] <- r[lev,i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k] + incs[[lev+3]][k,l] + incs[[lev+4]][l,m]
				                 }
				             }
				         }
				     }
				 }
		 	} else {
                #any level other than first
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
		             	for(l in 1:n){
			                 for(m in 1:n){
			                     payoffs[i,j,k,l,m] <- incs[[lev]][r5[lev-1],i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k] + incs[[lev+3]][k,l] + incs[[lev+4]][l,m]
			                 }
			             }
			         }
			     }
			 }
		 	}
            #pick max payoff
			 best <- which(payoffs == max(payoffs), arr.ind=T)
			 #sample if there is more than one option
			 r5[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
		 } else if (s-lev == 3){#if only 4 levels, do 4-level learner
			 payoffs<-array(0,dim=c(n,n,n,n))
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
						 for(l in 1:n){
							 payoffs[i,j,k,l] <- incs[[lev]][r5[lev-1],i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k] + incs[[lev+3]][k,l]
						 }
					 }
			     }
			 }
			 best <- which(payoffs == max(payoffs), arr.ind=T)
			 #sample if there is more than one option
             if(nrow(best) == 1){
                 r5[lev:(lev+3)] <- best
             }else{
                 r5[lev:(lev+3)] <- best[sample(nrow(best),1),]
             }
		 } else if (s-lev == 2){#if only 3 levels left, do 3-level learner
			 payoffs<-array(0,dim=c(n,n,n))
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
						 payoffs[i,j,k] <- incs[[lev]][r5[lev-1],i] + incs[[lev+1]][i,j] + incs[[lev+2]][j,k]
			        }
			     }
			 }
			 best <- which(payoffs == max(payoffs), arr.ind=T)
             if(nrow(best) == 1){
                 r5[lev:(lev+2)] <- best
             }else{
                 r5[lev:(lev+2)] <- best[sample(nrow(best),1),]
             }
		 } else if (s-lev == 1){#2-level learner
 			payoffs <- matrix(0, ncol= n, nrow=n)
 	        for (i in 1:n){
  	            for(j in 1:n){
  	                payoffs[i,j] <- incs[[lev]][r5[lev-1],i] + incs[[lev+1]][i,j]  
  	            }
  	        }
 	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r5[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r5[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])	 
		 } else if (s==lev){
            best <- which(incs[[lev]][r5[lev-1],] == max(incs[[lev]][r5[lev-1],]))
            r5[lev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
  	#payoff
  	tp <-r[1,r5[1]]
  	for (i in 2:s){
  		tp <- tp + incs[[i]][r5[i-1], r5[i]]
  	}
  	ans <- list(r5, tp)
  	return(ans)	
}