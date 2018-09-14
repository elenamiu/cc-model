#function to calculate penalties at each level
# penalties[[lev]][i,j] = penalties at level lev if trait i were chosen at level lev-1 and trait j were chosen at level lev
calculate_penalties <- function(s,n,r,a){
	penalties<- list()
	for (level in c(2:s)){#no penalty for level 1
		ps <- matrix(0,ncol=n,nrow=n)#ps[i,j] penalty if trait i was chosen at previous level and trait j is chosen at this level
		for (i in 1:n){
            #define dependency window
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
            #adjust payoff 
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
        #save adjusted payoffs for this level 
		penalties[[level]] <- ps
	}
	return(penalties)
}
##learners
#learning algorithms 
#consider x steps, take x steps
one_learner <- function(r,sdev){
	sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
    best <- which(r[1,]==max(r[1,]))
	repertoire[1] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],sdev)/dnorm(repertoire[i-1],repertoire[i-1],sdev)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	}
	#calculate payoff
	penalties<- calculate_penalties(s,n,r,a)
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	ans <- list(repertoire, tp)
	return(ans)
}

two_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
    #calculate penalties
	penalties<- calculate_penalties(s,n,r,a)
    #empty repertoire
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
    #payoffs[i,j] -  payoffs if trait i was chosen at level 1 and trait j was chosen at level 2
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
    #which payoff is best? array index - first and second trait
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	r2[2] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
    #from level 3 on 
	for (level in seq(3,s,2)){#for every pair of levels
	    if(level != s){#if there's at least two levels left, do 2-step learning
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
            #which trait combination maximised payoffs?
        	best <- which(payoffs == max(payoffs),arr.ind=T)
            #if more than one
	        r2[level] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	        r2[level+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
	    } else{#if only one level left, do one-shot learning
	        payoffs <- penalties[[level]][r2[level-1],]
            best <- which(payoffs == max(payoffs))
	        r2[level] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	    }
	}
	#calculate payoff
    #raw payoff for first level
	tp <-r[1,r2[1]]
	for (i in 2:s){
        # + adjusted payoff for all subsequent levels
		tp <- tp + penalties[[i]][r2[i-1], r2[i]]
	}
	ans <- list(r2, tp)
	return(ans)	
}

#3-level learner
three_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
    #calculate penalties
	penalties<- calculate_penalties(s,n,r,a)
	# 3-level maximiser
    #repertoire vector
	 r3 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s,3)){#for every triplet of levels
		 if(s-lev >=2){#if there's at least 3 levels left, do 3-level learning
			 if(lev==1){#if we're on the first level
				 payoffs<-array(0,dim=c(n,n,n)) 
                 #pyoffs[i,j,k] = payoffs if trait i was chosen at level 1, trait j was chosen at level 2, and trait k was chosen at level 3
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- r[lev,i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
				        }
				     }
				 }
                 #which combination of traits maximises the payoff
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 #sample if there is more than one option,
                 # save all three traits
                 if(nrow(best) == 1){
                     r3[lev:(lev+2)] <- best
                 }else{
                     r3[lev:(lev+2)] <- best[sample(nrow(best),1),]
                 }
			 }else{#from level 4 on 
				 payoffs<-array(0,dim=c(n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- penalties[[lev]][r3[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
				        }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 #sample if there is more than one option
                 if(nrow(best) == 1){
                     r3[lev:(lev+2)] <- best
                 }else{
                     r3[lev:(lev+2)] <- best[sample(nrow(best),1),]
                 }
			 }
		 }
		 else if(s-lev == 1) {#if only 2 levels left, do 2-level learning
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- penalties[[lev]][r3[lev-1],i] + penalties[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
            #if more than one, pick
        	r3[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
        	r3[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
		 }
		 else if(s==lev){#if only one level left, one-shot learner
			 best <- which(penalties[[lev]][r3[lev-1],] == max(penalties[[lev]][r3[lev-1],]))
			 r3[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
 	#payoff
 	tp <-r[1,r3[1]]
 	for (i in 2:s){
 		tp <- tp + penalties[[i]][r3[i-1], r3[i]]
 	}
 	ans <- list(r3, tp)
 	return(ans)	
}

#4-level learner
four_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties<- calculate_penalties(s,n,r,a)
	# 4-level maximiser
    #create empty repertoire
	 r4 <- matrix(0, ncol=s, nrow=1)
     #for each 4 levels
	 for (lev in seq(1,s,4)){
		 if(s-lev >=3){# if there's at least 4 levels left, do 4-level learning
			 if(lev == 1){# for first level, raw payoff + adjusted payoffs for levels 2-4
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
                 if(nrow(best) == 1){
                     r4[lev:(lev+3)] <- best
                 }else{
                     r4[lev:(lev+3)] <- best[sample(nrow(best),1),]
                 }
			 } else {
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
                 if(nrow(best) == 1){
                     r4[lev:(lev+3)] <- best
                 }else{
                     r4[lev:(lev+3)] <- best[sample(nrow(best),1),]
                 }
			 }
		 }
		 else if (s-lev == 2){#if there's only 3 levels left, do 3-level learning
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
		 }
		 else if(s-lev == 1) {#if only two levels left, do 2-level learning 
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- penalties[[lev]][r4[lev-1],i] + penalties[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
        	r4[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
        	r4[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
		 }
		 else if(s==lev){
			 best <- which(penalties[[lev]][r4[lev-1],] == max(penalties[[lev]][r4[lev-1],]))
			 r4[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		 }
	 }
 	#payoff
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
	 r5 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s,5)){
         if(s-lev >= 4){
    		 payoffs<-array(0,dim=c(n,n,n,n,n))
    		 if(lev == 1){
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
			# #sample if there is more than one option
			if(nrow(best) == 1){
			 r5[lev:(lev+4)] <- best
			}else{
			 r5[lev:(lev+4)] <- best[sample(nrow(best),1),]
			}
         }else if(s-lev == 3){
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
         }else if(s-lev==2){
			 payoffs<-array(0,dim=c(n,n,n))
			 for (i in 1:n){
			     for (j in 1:n){
		    		 for (k in 1:n){
						 payoffs[i,j,k] <- penalties[[lev]][r5[lev-1],i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
			        }
			     }
			 }
			 best <- which(payoffs == max(payoffs), arr.ind=T)
             if(nrow(best) == 1){
                 r5[lev:(lev+2)] <- best
             }else{
                 r5[lev:(lev+2)] <- best[sample(nrow(best),1),]
             }
         } else if(s-lev == 1){
  			payoffs <- matrix(0, ncol= n, nrow=n)
  	        for (i in 1:n){
   	            for(j in 1:n){
   	                payoffs[i,j] <- penalties[[lev]][r5[lev-1],i] + penalties[[lev+1]][i,j]  
   	            }
   	        }
  	        best <- which(payoffs == max(payoffs), arr.ind=T)
 	        r5[lev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
 	        r5[lev+1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])			
         } else if(s==lev){
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