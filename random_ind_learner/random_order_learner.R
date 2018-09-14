#############################################
## window learner that acquires traits in random order 
## x step learner - looks at x steps, learns 1
## when learning a previous trait, take into account what the agent already knows

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

#######################
## learning algorithms 
one_learner <-function(r,sdev){
	sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	repertoire <- matrix(0, ncol=s, nrow=1)
	to_learn <- sample(s)
	for (slev in to_learn){
		#if first level, just pick the best
		if (slev == 1){
			best <- which(r[slev,] == max(r[slev,]))
			repertoire[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		} else{
			#if the previous trait is known, take dependency into account
			if (repertoire[slev-1] > 0){
				dep <- dnorm(a,repertoire[slev-1],sdev)/dnorm(repertoire[slev-1],repertoire[slev-1],sdev)
				penalty <- r[slev,] * dep
				best <- which(penalty==max(penalty))
				repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			} else {
				#if the previous trait is not known, do not take dependency into account
				best <- which(r[slev,] == max(r[slev,]))
				repertoire[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			} 
		}
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

##
two_learner <- function(r, sdev){
	sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties <- calculate_penalties(s,n,r,a)
	#randomise levels
	to_learn <- sample(s)
	#repertoire vector
	r2 <- matrix(0,ncol=s,nrow=1)
	for (slev in to_learn){
		#if we're on the first trait 
		if(slev == 1){
            #if on the first level
			#we need to look ahead - if we know the second trait, we need to take that into account
			if (r2[slev+1]>0){
				payoffs <- matrix(0,ncol=n,nrow=1)
				for(i in 1:n){
					payoffs[i] <- r[1,i] + penalties[[2]][i,r2[slev+1]]
				}
				best <- which(payoffs == max(payoffs))
				r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			} 
			else{
				#if we don't know what the next one is, look at all
				payoffs <- matrix(0, ncol=n, nrow=n)
				for(i in 1:n){
					for(j in 1:n){
						payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]
					}
				}
				best <- which(payoffs == max(payoffs),arr.ind=T)
				#sample if there is more than one, keep first 
				r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
			}
		} 
		else{#if we're not on the first level 
			#if we know the level before, take dependencies into account
			if(r2[slev-1] > 0){
				if (slev == s){#if we're at the last level
					dep <- dnorm(a,r2[slev-1],sdev)/dnorm(r2[slev-1],r2[slev-1],sdev)
					penalty <- r[slev,] * dep
					best <- which(penalty==max(penalty))
					r2[slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{#if we're not at the last level
					#if we know the next trait, we need to take it into account for payoff
					if(r2[slev+1]>0){
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- penalties[[slev]][r2[slev-1],i] + penalties[[slev+1]][i,r2[slev+1]]
						}
					best <- which(payoffs == max(payoffs))
					r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					} 
					else if(slev !=s ){#if we don't know the next trait, look at all
						payoffs <- matrix(0, ncol=n, nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- penalties[[slev]][r2[slev-1],i] + penalties[[slev+1]][i,j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			} 
			else{#if we don't know the level before, don't take dependencies into account
				if (slev == s){#if we're on the last level
					best <- which(r[slev,] == max(r[slev,]))
					r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{#if we're not on the last level
					#do we know the next level?
					if(r2[slev+1]>0){
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- r[slev,i] + penalties[[slev+1]][i,r2[slev+1]]
						}
						best <- which(payoffs == max(payoffs))
						r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					} 
					else if(slev !=s){#if we don't know the next level
						payoffs <- matrix(0, ncol=n, nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][i,j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}			
		}
	}
	#calculate payoff
	tp <-r[1,r2[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][r2[i-1], r2[i]]
	}
	ans <- list(r2, tp)
	return(ans)	
}

##
three_learner <- function(r, sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	#calculate adjusted payoffs 
	penalties<- calculate_penalties(s,n,r,a)
	#randomise levels
	to_learn <- sample(s)
	#create empty repertoire
	r3 <- matrix(0, ncol=s, nrow=1)
	for (slev in to_learn){
		if (slev == 1){
			#if on the first level
			if(r3[slev+1] > 0){
				#if we know the 2nd level
				if(r3[slev+2]>0){
					#if the know the 3rd level
					payoffs <- matrix(0,ncol=n,nrow=1)
					for(i in 1:n){
						payoffs[i] <- r[1,i] + penalties[[2]][i,r3[slev+1]] + penalties[[3]][r3[slev+1],r3[slev+2]]
					}
					best <- which(payoffs == max(payoffs))
					r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{
					# if we don't know the 3rd level
					payoffs <- matrix(0, ncol=n, nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- r[1,i] + penalties[[2]][i,r3[slev+1]] + penalties[[3]][r3[slev+1],j]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					#sample if there is more than one, keep first 
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			} else{
				#if we don't know the second level
				if(r3[slev+2]>0){
					#if we know the 3rd level
					payoffs <- matrix(0, ncol=n, nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,r3[slev+2]]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					#sample if there is more than one, keep first 
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
				else{
					#if we don't know the 3rd level 
					payoffs <- array(0, dim=c(n,n,n))
					for(i in 1:n){
						for(j in 1:n){
							for(k in 1:n){
								payoffs[i,j,k] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,k]
							}
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					#sample if there is more than one, keep first 
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
		}
		else if(s-slev>=2){
			#if there are at least three levels left
			if (r3[slev-1] == 0){
				#if we don't know the previous level
				if(r3[slev+1] > 0){
					#if we know the next level
					if(r3[slev+2]>0){
						#if the know the 3rd level
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- r[slev,i] + penalties[[slev+1]][i,r3[slev+1]] + penalties[[slev+2]][r3[slev+1],r3[slev+2]]
						}
						best <- which(payoffs == max(payoffs))
						r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
					else{
						# if we don't know the 3rd level
						payoffs <- matrix(0, ncol=n, nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][i,r3[slev+1]] + penalties[[slev+2]][r3[slev+1],j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				} else{
					#if we don't know the next level
					if(r3[slev+2]>0){
						#if we know the 3rd level
						payoffs <- matrix(0, ncol=n, nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][i,j] + penalties[[slev+2]][j,r3[slev+2]]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the 3rd level 
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){
							for(j in 1:n){
								for(k in 1:n){
									payoffs[i,j,k] <- r[slev,i] + penalties[[slev+1]][i,j] + penalties[[slev+2]][j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}
			else{
				#if we know the previous level
				if(r3[slev+1] > 0){
					#if we know the next level
					if(r3[slev+2]>0){
						#if the know the 3rd level
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- penalties[[slev]][r3[slev-1],i] + penalties[[slev+1]][i,r3[slev+1]] + penalties[[slev+2]][r3[slev+1],r3[slev+2]]
						}
						best <- which(payoffs == max(payoffs))
						r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
					else{
						# if we don't know the 3rd level
						payoffs <- matrix(0, ncol=n, nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- penalties[[slev]][r3[slev-1],i] + penalties[[slev+1]][i,r3[slev+1]] + penalties[[slev+2]][r3[slev+1],j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				} else{
					#if we don't know the next level
					if(r3[slev+2]>0){
						#if we know the 3rd level
						payoffs <- matrix(0, ncol=n, nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- penalties[[slev]][r3[slev-1],i]  + penalties[[slev+1]][i,j] + penalties[[slev+2]][j,r3[slev+2]]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the 3rd level 
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){
							for(j in 1:n){
								for(k in 1:n){
									payoffs[i,j,k] <- penalties[[slev]][r3[slev-1],i]  + penalties[[slev+1]][i,j] + penalties[[slev+2]][j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						#sample if there is more than one, keep first 
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}		
			}
		}
		else if (s-slev == 1){
			#if there are only two levels left
			if (r3[slev-1] == 0){
				#if we don't know the previous level
				if(r3[slev+1]>0){
					#if we know the next level
					payoffs <- matrix(0,ncol=n,nrow=1)
					for(i in 1:n){
						payoffs[i] <- r[slev,i] + penalties[[slev+1]][i,r3[slev+1]]
					}
					best <- which(payoffs == max(payoffs))
					r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{
					#if we don't know the next level
					payoffs <- matrix(0, ncol=n, nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][i,j]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					#sample if there is more than one, keep first 
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
			else{
				#if we know the previous level
				if(r3[slev+1]>0){
					#if we know the next level
					payoffs <- matrix(0,ncol=n,nrow=1)
					for(i in 1:n){
						payoffs[i] <- penalties[[slev]][r3[slev-1],i] + penalties[[slev+1]][i,r3[slev+1]]
					}
					best <- which(payoffs == max(payoffs))
					r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{
					#if we don't know the next level
					payoffs <- matrix(0, ncol=n, nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- penalties[[slev]][r3[slev-1],i] + penalties[[slev+1]][i,j]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					#sample if there is more than one, keep first 
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
		}
		else if(slev==s){
			#if on the last level
			if (r3[slev-1] == 0){
				#if we don't know the previous level
				best <- which(r[slev,] == max(r[slev,]))
				r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else{
				#if we know the previous level
				best <- which(penalties[[slev]][r3[slev-1],] == max(penalties[[slev]][r3[slev-1],]))
				r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
		}			
	}
	#calculate payoff
	tp <-r[1,r3[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][r3[i-1], r3[i]]
	}
	ans <- list(r3, tp)
	return(ans)	
}