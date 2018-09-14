###################################
## WINDOW LEARNER - check x steps, take one

#function to calculate penalties at each level
# penalties[[lev]][i,j] = penalties at level lev if trait i were chosen at level lev-1 and trait j were chosen at level lev
calculate_penalties <- function(s,n,r,a){
	penalties<- list()
	for (level in c(2:s)){#no dependency for level 1
		#ps[i,j] - penalties if trait i was chosen at previous level and trai j is chosen now
		ps <- matrix(0,ncol=n,nrow=n)
		#for each trait i chosen at previous level
		for (i in 1:n){
			#define dependency window
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
            #a is just a vector 1:n that makes it easier to calculate payoffs for all potentially chosen traits in one go
			#calculate adjusted payoff for all traits in this level
            #base payoff for this level x adjusted payoff acccording to what was chosen at previous level
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		#save adjusted payoffs for this level
		penalties[[level]] <- ps
	}
	return(penalties)
}
####
# learning algorithms 
# consider x steps, only take one at a time
one_learner <- function(r,sdev){
	#values for sd, number of traits, number of levels
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	#all traits - makes it easier to calculate payoffs
	a <- 1:n
	#create empty repertoire
	repertoire <- matrix(0,ncol=s,nrow=1) 
	#pick trait for first level - max payoff
	best <- which(r[1,] == max(r[1,]))
	#if more than one, sample from however many there are, otherwise first
	repertoire[1] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	#for all other levels
	for(i in 2:s){
		#calculate dependency window
		dep <- dnorm(a,repertoire[i-1],sdev)/dnorm(repertoire[i-1],repertoire[i-1],sdev)
		#calculate adjusted payoff - current level * dependency window
		penalty <- r[i,] * dep
		#pick best payoff
		best <- which(penalty==max(penalty))
		#sample if more than one
		repertoire[,i] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	}
	#calculate total payoff
	penalties<- calculate_penalties(s,n,r,a)
	#payoff for trait at level 1
	tp <-r[1,repertoire[1]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	ans <- list(repertoire, tp)
	return(ans)
}

two_learner <- function(r, sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	#calculate penalties
	penalties<- calculate_penalties(s,n,r,a)
	#create empty repertoire vector
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	#payoffs[i,j] = payoff if trait i were chosen at level 1 and trait j were chosen at level 2
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	#which payoff is best? array index - traits i and j, keep only first 
	best <- which(payoffs == max(payoffs),arr.ind=T)
	#sample if there is more than one, keep first 
	r2[1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	#for all other levels 
	for(level in seq(2,s)){
		#if there's more than one level, do two-step learning
		if(level != s){
			#payoff[i,j] = payoff if trait i were chosen at level level and trait j were chosen at level+1
			payoffs <- matrix(0, ncol=n, nrow=n)
			for(i in 1:n){
				for(j in 1:n){
					payoffs[i,j] <-penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]
				}
			}
			#which is highest?
			best <- which(payoffs == max(payoffs), arr.ind=T)
			#sample if more than one
			r2[level] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
		}else {
			#if at the last level, do one step learning
			payoffs <- penalties[[level]][r2[level-1],]
			best <- which(payoffs == max(payoffs))
			r2[level] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
	}
	#calculate payoff = payoff for first trait in repertoire + adjusted payoffs for all the others
	tp <-r[1,r2[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][r2[i-1], r2[i]]
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
	#calculate adjusted payoffs 
	penalties<- calculate_penalties(s,n,r,a)
	# 3-level maximiser
	#create empty repertoire
	 r3 <- matrix(0, ncol=s, nrow=1)
	 #for each level
	 for (lev in seq(1,s)){
		 # if at least 3 levels left, do 3-step learning 
		 if(s-lev >=2){
			 #if it's the first level = payoff at level 1 + adjusted payoff at 2 and 3
			 if(lev==1){
				 #payoffs[i,j,k] = payoff if trait i was chosen at level 1, trait j at level 2, and trait k at level 3
				 payoffs<-array(0,dim=c(n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- r[lev,i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
				        }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 # #sample if there is more than one option
				 r3[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 }else{
				 #if not at the first level, calculate payoff for 3 successive levels
				 payoffs<-array(0,dim=c(n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- penalties[[lev]][r3[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
				        }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 # #sample if there is more than one option
				 r3[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 }
		 }
		 else if(s-lev == 1) {#if there are two left, do two level learnign
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- penalties[[lev]][r3[lev-1],i] + penalties[[lev+1]][i,j]  
 	            }
 	        }
			#which one is max
	        best <- which(payoffs == max(payoffs), arr.ind=T)
			#if more than one, sample 
	        r3[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r3[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
		 }
		 else if(s==lev){
			 #if only one level left, do one step learning
			 best <- which(penalties[[lev]][r3[lev-1],] == max(penalties[[lev]][r3[lev-1],]))
			 r3[lev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
  	#calculate total payoffs
  	tp <-r[1,r3[1]]
  	for (i in 2:s){
  		tp <- tp + penalties[[i]][r3[i-1], r3[i]]
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
	penalties<- calculate_penalties(s,n,r,a)
	# 4-level maximiser
	#create empty repertoire
	 r4 <- matrix(0, ncol=s, nrow=1)
	 #for every level
	 for (lev in seq(1,s)){
		 #if there's at least 4 levels left, do 4-level learning
		 if(s-lev >=3){
			 #if on the first level, raw payoff+adjusted payoffs
			 if(lev == 1){
				 payoffs<-array(0,dim=c(n,n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 for(l in 1:n){
								 payoffs[i,j,k,l] <- r[lev,i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k] + penalties[[lev+3]][k,l]
							 }
						 }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 #sample if there is more than one option
				 # row <- sample(1:nrow(best),1)
				 r4[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			 } else {
				 #if we're past level 1, 4-step learning 
				 payoffs<-array(0,dim=c(n,n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 for(l in 1:n){
								 payoffs[i,j,k,l] <- penalties[[lev]][r4[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k] + penalties[[lev+3]][k,l]
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
						 payoffs[i,j,k] <- penalties[[lev]][r4[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
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
			 #if only two levels left, do 2-step learning
		 } else if(s-lev == 1) {
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- penalties[[lev]][r4[lev-1],i] + penalties[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
			#if more than one, sample
	        r4[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r4[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
		#if only one level left, do one shot learning
		 } else if(s==lev){
			 best <- which(penalties[[lev]][r4[lev-1],] == max(penalties[[lev]][r4[lev-1],]))
			 r4[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
	 #calculate payoff 
  	tp <-r[1,r4[1]]
  	for (i in 2:s){
  		tp <- tp + penalties[[i]][r4[i-1], r4[i]]
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
	penalties<- calculate_penalties(s,n,r,a)
	# 5-level maximiser
	#create empty repertoire
	 r5 <- matrix(0, ncol=s, nrow=1)
	 #for each level
	 for (lev in seq(1,s)){
		 #if there's at least 5 levels, do 5-level learning
		 if(s - lev >=4){
			 payoffs<-array(0,dim=c(n,n,n,n,n))
			 #special case if we're on the first level
			 if(lev == 1){
				 #5-level for loop :(
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
			             	for(l in 1:n){
				                 for(m in 1:n){
				                     payoffs[i,j,k,l,m] <- r[lev,i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k] + penalties[[lev+3]][k,l] + penalties[[lev+4]][l,m]
				                 }
				             }
				         }
				     }
				 }
		 	} else {
			#if we're past the 1st level
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
		             	for(l in 1:n){
			                 for(m in 1:n){
			                     payoffs[i,j,k,l,m] <- penalties[[lev]][r5[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k] + penalties[[lev+3]][k,l] + penalties[[lev+4]][l,m]
			                 }
			             }
			         }
			     }
			 }
		 	}
			 best <- which(payoffs == max(payoffs), arr.ind=T)
			 #sample if there is more than one option
			 r5[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
		 } else if (s-lev == 3){
			 #if there are only 4 levels left, do 4-step learning
			 payoffs<-array(0,dim=c(n,n,n,n))
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
						 for(l in 1:n){
							 payoffs[i,j,k,l] <- penalties[[lev]][r5[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k] + penalties[[lev+3]][k,l]
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
		 # if only 3 levels left, do 3-level learning 
		 } else if (s-lev == 2){
			 payoffs<-array(0,dim=c(n,n,n))
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
						 payoffs[i,j,k] <- penalties[[lev]][r5[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
			        }
			     }
			 }
			 #pick best
			 best <- which(payoffs == max(payoffs), arr.ind=T)
			 #if more than one, sample
             if(nrow(best) == 1){
                 r5[lev:(lev+2)] <- best
             }else{
                 r5[lev:(lev+2)] <- best[sample(nrow(best),1),]
             }
		#if only 2 levels left, do 2-level learning
		 } else if (s-lev == 1){
 			payoffs <- matrix(0, ncol= n, nrow=n)
 	        for (i in 1:n){
  	            for(j in 1:n){
  	                payoffs[i,j] <- penalties[[lev]][r5[lev-1],i] + penalties[[lev+1]][i,j]  
  	            }
  	        }
			#pick best
 	        best <- which(payoffs == max(payoffs), arr.ind=T)
			#if more than one, sample
	        r5[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r5[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])			
		#if only one level left, one-step learning	 
		 } else if (s==lev){
			 best <- which(penalties[[lev]][r5[lev-1],] == max(penalties[[lev]][r5[lev-1],]))
			 r5[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
  	#payoff
  	tp <-r[1,r5[1]]
  	for (i in 2:s){
  		tp <- tp + penalties[[i]][r5[i-1], r5[i]]
  	}
  	ans <- list(r5, tp)
  	return(ans)	
}