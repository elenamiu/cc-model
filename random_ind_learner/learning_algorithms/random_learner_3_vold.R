#############################################
## window learner that acquires traits in random order 
## x step learner - looks at x steps, learns 1
## when learning a previous trait, take into account what the agent already knows
## THREE STEP DEPENDENCIES
# penalties[[lev]][i,j,k] = penalties at levels lev if trait i was chosen at lev-2, trait h was chosen at lev-1 and trait k was chosen at level lev

#################
## they look forward, but they do take into account what they already know

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

#######################
## learning algorithms 
one_learner <-function(r,sdev){
	sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties<- calculate_penalties(s,n,r,a)
	to_learn <- sample(s)
	repertoire <- matrix(0, ncol=s, nrow=1)
	for (slev in to_learn){
		#if first level, just pick the best
		if (slev == 1){
			best <- which(r[1,] == max(r[1,]))
			repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
		else if (slev == 2){
			if(repertoire[slev-1]>0){
				#if we know the previous level
				penalty <- penalties[[slev]][repertoire[slev-1],]#this is a vector
				best <- which(penalty==max(penalty))
				repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else if(repertoire[slev-1]==0){
				#if we don't know the previous level
				best <- which(r[slev]==max(r[slev]))
				repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
		} 
		else{
			# if higher than second level and previous levels are known, take dependency into account
			if(repertoire[slev-1] > 0){
				#if we know the prev level
				if(repertoire[slev-2] >0){
					#if we know two levels ago
					penalty <- penalties[[slev]][repertoire[slev-2],repertoire[slev-1],]#this is a vector
					best <- which(penalty==max(penalty))
					repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{
					#if we don't know two levels ago
					penalty <- penalties[[slev]][,repertoire[slev-1],]#this is a matrix
					best <- which(penalty==max(penalty),arr.ind=T)
					#save the second of the matrix which, I hope, is the value for the current level
					repertoire[,slev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
				}
			}
			else{
				#if we don't know the previous level
				if(repertoire[slev-2]>0){
					#if we know two levels ago
					penalty <- penalties[[slev]][repertoire[slev-2],,]#this is a matrix
					best <- which(penalty==max(penalty),arr.ind=T)
					repertoire[,slev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
				}
				else{
					#if we don't know two levels ago
					penalty <- penalties[[slev]]
					best <- which(penalty==max(penalty),arr.ind=T)
					repertoire[,slev] <-ifelse(nrow(best)>1,best[sample(nrow(best),1),3], best[1,3])
				}
			}
		}
	}
	#calculate total payoff
	#payoff for trait at level 1
	tp <-r[1,repertoire[1]] + penalties[[2]][repertoire[1],repertoire[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][repertoire[i-2],repertoire[i-1], repertoire[i]]
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
		if(slev == 1){
			#if on the first level
			if(r2[slev+1]>0){
				# if we know the next trait, take into account
				payoffs <- matrix(0,ncol=n,nrow=1)
				for(i in 1:n){
					payoffs[i] <- r[1,i] + penalties[[2]][i,r2[slev+1]]
				}
				best <- which(payoffs == max(payoffs))
				r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else{
				#if we don't know the next trait
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
		else if(slev == 2){
			#if on the second level
			if(r2[slev+1]>0){
				#if we know the next trait
				payoffs <- matrix(0,ncol=n,nrow=1)
				for(i in 1:n){
					if(r2[slev-1]>0){
						#if we know the previous level
						factor1 <- penalties[[2]][r2[slev-1],i]
					}
					else{
						#if we don't know the previous level
						factor1 <- r[slev,i]
					}
					payoffs[i] <- factor1 + penalties[[3]][i,r2[slev+1]]
				}
				best <- which(payoffs == max(payoffs))
				r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else {
				#if we don't know the next trait
				payoffs <- matrix(0,ncol=n,nrow=n)
				for(i in 1:n){
					if(r2[slev-1]>0){
						#if we know the previous level
						factor1 <- penalties[[2]][r2[slev-1],i]
					}
					else{
						#if we don't know the previous level
						factor1 <- r[slev,i]
					}
					for(j in 1:n){
						payoffs[i,j] <- factor1 + penalties[[3]][i,j]
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					#sample if there is more than one, keep first 
					r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
		}
		else if(s-slev > 1){
			#if there are at least two levels left 
			
		}
		else if(slev == s){
			# if on the last level, do one level learning
		}
	}



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
	r3 <- matrix(0, ncol=s, nrow=1)
	for (slev in to_learn){
		if(slev == 1){
			#if first level
		}
		else if(slev == 2){
			#if second level
		}
		else 
	}


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