###################################
## WINDOW LEARNER - looks ahead x steps, take one
## THREE LEVEL DEPENDENCIES

##calculate adjusted payoffs at each level
#penalties[[lev]][i,j,k] = penalties at levels lev if trait i was chosen at lev-2, trait h was chosen at lev-1 and trait k was chosen at level lev
calculate_penalties <- function(s,n,r,a){
	penalties <- list()
	#for level 2, just a one-step dependency
	ps <- matrix(0,ncol=n,nrow=n)
	#for each trait i chosen at level 1
	for(i in 1:n){
		dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
		penalty <- r[2,]*dep
		ps[i,] <- penalty
	}
	penalties[[2]] <- ps
	#from level 3 on
	for(level in c(3:s)){
		ps <- array(0,dim=c(n,n,n))
		# for each trait i chosen two levels ago and for each trai j chosen at previous level
		for(i in 1:n){
			dep1 <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
			for(j in 1:n){
				dep2 <- dnorm(a,j,sdev)/dnorm(j,j,sdev)
				penalty <- r[level,] * dep1 * dep2
				ps[i,j,] <- penalty
			}
		}
		penalties[[level]] <- ps
	}
	return(penalties)
}

####
# learning algorithms 
# consider x steps, only take one at a time
##### IS THIS RIGHT? Take dependency into account or not??

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
	penalties<- calculate_penalties(s,n,r,a)
	for (i in 2:s){
		#get adjusted payoffs
		penalty <- penalties[[i]]
		#which is best
		best <- which(penalty==max(penalty), arr.ind=T)
		repertoire[,i]<- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])####should this be the last one???
	}

	#payoff for trait at level 1
	tp <-r[1,repertoire[1]] + penalties[[2]][repertoire[1],repertoire[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][repertoire[i-2],repertoire[i-1], repertoire[i]]
	}
	ans <- list(repertoire, tp)
	return(ans)
}

two_learner <- function(r, sdev){
	#look ahead two levels
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
	
	#second level
	#payoffs[i,j] if trait i was chosen at level 2 and trait j was chosen at level 3
	payoffs <- matrix(0, ncol=n, nrow=n)
	for(i in 1:n){
		for(j in 1:n){
			payoffs[i,j] <- penalties[[2]][r2[1],i] + penalties[[3]][r2[1],i,j]
		}
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[2] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	#for all other levels 
	for(level in seq(3,s)){
		#if there are at least two levels left, do 2-step learning
		if(s-level >=1){
			#payoffs[i,j] - if trait i was chosen at level and trait j was chosen at level+1
			payoffs <- array(0,dim=c(n,n)) 
			for(i in 1:n){
				for(j in 1:n){
					payoffs[i,j] <- penalties[[level]][r2[level-2],r2[level-1],i] + penalties[[level+1]][r2[level-1],i,j]
				}
			}
			#which is the best
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r2[level] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1])
		}else {
			#if one more level left, do one level learning
			payoffs <- penalties[[level]][r2[level-2],r2[level-1],]
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r2[level] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])	
		}
	}
	#calculate payoff = payoff for first trait in repertoire + adjusted payoffs for all the others
	tp <-r[1,r2[1]] + penalties[[2]][r2[1],r2[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][r2[i-2],r2[i-1], r2[i]]
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
	#for level 1
 	payoffs <- array(0,dim=c(n,n,n))
 	for (i in 1:n){
 	    for (j in 1:n){
			for(k in 1:n){
	 	        payoffs[i,j,k] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][i,j,k]
			}
 	    }
 	}
 	#which payoff is best? array index - traits i and j, keep only first 
 	best <- which(payoffs == max(payoffs),arr.ind=T)
 	#sample if there is more than one, keep first 
 	r3[1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	
	#for level 2
	payoffs <- array(0, dim=c(n,n,n))
	for (i in 1:n){
		for (j in 1:n){
			for(k in 1:n){
				payoffs[i,j,k] <- penalties[[2]][r3[1],i] +penalties[[3]][r3[1],i,j] + penalties[[4]][i,j,k]
			}
		}
	}
	best <- which(payoffs == max(payoffs), arr.ind=T)
	r3[2] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	
	#for other levels
	for(lev in seq(3,s)){
		if (s-lev >=2){
			payoffs <- array(0,dim=c(n,n,n))
			for (i in 1:n){
				for (j in 1:n){
					for (k in 1:n){
						payoffs[i,j,k] <- penalties[[lev]][r3[lev-2],r3[lev-1],i] + penalties[[lev+1]][r3[lev-1],i,j] + penalties[[lev+2]][i,j,k]
					}
				}
			}
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r3[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
		}else if(s-lev ==1){
			#if only two levels left, do two-step learning
			payoffs <- array(0,dim=c(n,n)) 
			for(i in 1:n){
				for(j in 1:n){
					payoffs[i,j] <- penalties[[lev]][r3[lev-2],r3[lev-1],i] + penalties[[lev+1]][r3[lev-1],i,j]
				}
			}
			#which is the best
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r3[lev] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1])
		}
		else {
			#one level left, do one step learning
			payoffs <- penalties[[lev]][r3[lev-2],r3[lev-1],]
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r3[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
	}
  	#calculate total payoffs
	tp <-r[1,r3[1]] + penalties[[2]][r3[1],r3[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][r3[i-2],r3[i-1], r3[i]]
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
	#for level 1
 	payoffs <- array(0,dim=c(n,n,n,n))
 	for (i in 1:n){
 	    for (j in 1:n){
			for(k in 1:n){
				for(l in 1:n){
		 	        payoffs[i,j,k,l] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][i,j,k] + penalties[[4]][j,k,l]
				}
			}
 	    }
 	} 
 	best <- which(payoffs == max(payoffs),arr.ind=T)
 	#sample if there is more than one, keep first 
 	r4[1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	#for level 2
	payoffs <- array(0, dim=c(n,n,n,n))
	for(i in 1:n){
		for(j in 1:n){
			for(k in 1:n){
				for(l in 1:n){
					payoffs[i,j,k,l] <- penalties[[2]][r4[1],i] + penalties[[3]][r4[1],i,j] + penalties[[4]][i,j,k] + penalties[[5]][j,k,l]
				}
			}
		}
	}
	best <- which(payoffs == max(payoffs), arr.ind=T)
	#sample if there is more than one 
 	r4[2] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	#for all other levels
	for(lev in seq(3,s)){
		if(s-lev >=3){
			#if at least 4 levels left, do 4 level learning
			payoffs <- array(0, dim=c(n,n,n,n))
			for(i in 1:n){
				for(j in 1:n){
					for(k in 1:n){
						for(l in 1:n){
							payoffs[i,j,k,l] <- penalties[[lev]][r4[lev-2],r4[lev-1],i] + penalties[[lev+1]][r4[lev-1],i,j] + penalties[[lev+2]][i,j,k] + penalties[[lev+3]][j,k,l]
						}
					}
				}
			}
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r4[lev] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1]) 
		} else if(s-lev ==2){
			#if only 3 levels left, do 3 level learning
			payoffs <- array(0,dim=c(n,n,n))
			for (i in 1:n){
				for (j in 1:n){
					for (k in 1:n){
						payoffs[i,j,k] <- penalties[[lev]][r4[lev-2],r4[lev-1],i] + penalties[[lev+1]][r4[lev-1],i,j] + penalties[[lev+2]][i,j,k]
					}
				}
			}
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r4[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
		} else if(s-lev ==1){
			#if two levels left, do two level learning
			payoffs <- array(0,dim=c(n,n)) 
			for(i in 1:n){
				for(j in 1:n){
					payoffs[i,j] <- penalties[[lev]][r4[lev-2],r4[lev-1],i] + penalties[[lev+1]][r4[lev-1],i,j]
				}
			}
			#which is the best
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r4[lev] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1])
		} else {
			#if only one level left, do one level learning
			payoffs <- penalties[[lev]][r4[lev-2],r4[lev-1],]
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r4[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
	}
  	#calculate total payoffs
	tp <-r[1,r4[1]] + penalties[[2]][r4[1],r4[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][r4[i-2],r4[i-1], r4[i]]
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
	#for level 1
 	payoffs <- array(0,dim=c(n,n,n,n,n))
 	for (i in 1:n){
 	    for (j in 1:n){
			for(k in 1:n){
				for(l in 1:n){
					for(m in 1:n){
			 	        payoffs[i,j,k,l,m] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][i,j,k] + penalties[[4]][j,k,l] + penalties[[5]][k,l,m]
					}
				}
			}
 	    }
 	} 
 	best <- which(payoffs == max(payoffs),arr.ind=T)
 	#sample if there is more than one, keep first 
 	r5[1] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	#for level 2
	payoffs <- array(0, dim=c(n,n,n,n,n))
	for(i in 1:n){
		for(j in 1:n){
			for(k in 1:n){
				for(l in 1:n){
					for(m in 1:n){
						payoffs[i,j,k,l,m] <- penalties[[2]][r5[1],i] + penalties[[3]][r5[1],i,j] + penalties[[4]][i,j,k] + penalties[[5]][j,k,l] + penalties[[6]][k,l,m]
					}
				}
			}
		}
	}
	best <- which(payoffs == max(payoffs), arr.ind=T)
	#sample if there is more than one 
 	r5[2] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
	#for all other levels
	for(lev in seq(3,s)){
		if(s-lev >=4){
			#if at least 5 levels left, do 5 level learning
			payoffs <- array(0, dim=c(n,n,n,n,n))
			for(i in 1:n){
				for(j in 1:n){
					for(k in 1:n){
						for(l in 1:n){
							for(m in 1:n){
								payoffs[i,j,k,l,m] <- penalties[[lev]][r5[lev-2],r5[lev-1],i] + penalties[[lev+1]][r5[lev-1],i,j] + penalties[[lev+2]][i,j,k] + penalties[[lev+3]][j,k,l] + penalties[[lev+4]][k,l,m]
							}
						}
					}
				}
			}
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r5[lev] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1]) 
		} else if(s-lev == 3){
			#if only 4 levels left, do 4 level learning
			payoffs <- array(0, dim=c(n,n,n,n))
			for(i in 1:n){
				for(j in 1:n){
					for(k in 1:n){
						for(l in 1:n){
							payoffs[i,j,k,l] <- penalties[[lev]][r5[lev-2],r5[lev-1],i] + penalties[[lev+1]][r5[lev-1],i,j] + penalties[[lev+2]][i,j,k] + penalties[[lev+3]][j,k,l]
						}
					}
				}
			}
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r5[lev] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1]) 	
		} else if(s-lev ==2){
			#if only 3 levels left, do 3 level learning
			payoffs <- array(0,dim=c(n,n,n))
			for (i in 1:n){
				for (j in 1:n){
					for (k in 1:n){
						payoffs[i,j,k] <- penalties[[lev]][r5[lev-2],r5[lev-1],i] + penalties[[lev+1]][r5[lev-1],i,j] + penalties[[lev+2]][i,j,k]
					}
				}
			}
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r5[lev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
		} else if(s-lev ==1){
			#if two levels left, do two level learning
			payoffs <- array(0,dim=c(n,n)) 
			for(i in 1:n){
				for(j in 1:n){
					payoffs[i,j] <- penalties[[lev]][r5[lev-2],r5[lev-1],i] + penalties[[lev+1]][r5[lev-1],i,j]
				}
			}
			#which is the best
			best <- which(payoffs==max(payoffs), arr.ind=T)
			#sample if more than one
			r5[lev] <- ifelse(nrow(best)>1, best[sample(nrow(best),1),1], best[1,1])
		} else {
			#if only one level left, do one level learning
			payoffs <- penalties[[lev]][r5[lev-2],r5[lev-1],]
			best <- which(payoffs == max(payoffs), arr.ind=T)
			r5[lev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
	}
  	#calculate total payoffs
	tp <-r[1,r5[1]] + penalties[[2]][r5[1],r5[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][r5[i-2],r5[i-1], r5[i]]
	}
  	ans <- list(r5, tp)
  	return(ans)	
}