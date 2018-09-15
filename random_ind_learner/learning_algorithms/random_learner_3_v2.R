#############################################
## window learner that acquires traits in random order 
## x step learner - looks at x steps, learns 1
## when learning a previous trait, take into account what the agent already knows
## THREE STEP DEPENDENCIES
# penalties[[lev]][i,j,k] = penalties at levels lev if trait i was chosen at lev-2, trait h was chosen at lev-1 and trait k was chosen at level lev

#DON'T LOOK BACK - AT ALL
# if we know both previous levels, take them into account, otherwise learn from scratch
# ALSO - if we don't know the previous level, learn from 


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
		if(slev == 1){
			#just pick the best, ignore penalties
			best <- which(r[slev,] == max(r[slev,]))
			repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
		}
		else if(slev==2){
			#if we're on the 2nd level
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
			#if we're past the second level
			if(repertoire[slev-1]>0 & repertoire[slev-2]>0){
				#if we know both previous levels
				penalty <- penalties[[slev]][repertoire[slev-2],repertoire[slev-1],]
				best <- which(penalty==max(penalty))
				repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else{
				#if we don't know both, learn from scratch, just look for max
				best <- which(r[slev,] == max(r[slev,]))
				repertoire[,slev] <-ifelse(length(best)>1,best[sample(length(best),1)], best[1])
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
	r2 <- matrix(0,ncol=n,nrow=1)
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
		else if(slev == 2){#if on the second level
			if(r2[slev+1]>0){
				#if we know the next trait
				if(r2[slev-1]>0){
					#if we know the previous level
					payoffs <- matrix(0,ncol=n,nrow=1)
					for(i in 1:n){
						payoffs[i] <- penalties[[2]][r2[slev-1],i] + penalties[[3]][r2[slev-1],i,r2[slev+1]]
					}
					best <- which(payoffs == max(payoffs))
					r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
				}
				else{
					#if we don't know the previous level
					payoffs <- matrix(0,ncol=n,nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- r[slev,i] + penalties[[3]][j,i,r2[slev+1]]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
			else {
				#if we don't know the next trait
				if(r2[slev-1]>0){
					#if we know the previous level
					payoffs <- matrix(0,ncol=n,nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- penalties[[2]][r2[slev-1],i] + penalties[[3]][r2[slev-1],i,j]
						}
					}
					best <- which(payoffs == max(payoffs), arr.ind=T)
					r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
				else{
					#if we don't know the previous level
                    ## IF WE DON'T KNOW THE PREVIOUS LEVEL, CAN'T TAKE DEPENDENCIES INTO ACCOUNT FOR NEXT LEVEL because 
                    ## payoff[next_lev] = penalties[[next_lev]][prev_l, this_l, next_l] but we don't know prev_l
                    ## instead, use just the raw payoff for next level
					payoffs <- array(0,dim=c(n,n))
					for(i in 1:n){#trait for this level
						for(j in 1:n){#trait for next level
							payoffs[i,j] <- r[slev,i] + r[slev+1,j]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
		}
		else if(s-slev>=1){
			#if past second level
			if(r2[slev+1]>0){
				#if we know the next trait
				if(r2[slev-1]>0 & r2[slev-2]>0){
					#if we know the previous two levels
					payoffs <- matrix(0,ncol=n,nrow=1)
					for(i in 1:n){
							payoffs[i] <- penalties[[slev]][r2[slev-2],r2[slev-1],i] + penalties[[slev+1]][r2[slev-1],i,r2[slev+1]]
						}
						best <- which(payoffs == max(payoffs))
						r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
				else{
					#if we don't know both the previous levels, learn from scratch (kind of?)
					if(r2[slev-1]>0){
                        #if we know the previous level
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- r[slev,i] + penalties[[slev+1]][r2[slev-1],i,r2[slev+1]]
						}
						best <- which(payoffs == max(payoffs))
						r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
					else if(r2[slev-1]==0){
                        #if we don't know the previous level
                        #don't take dependencies into account
						payoffs<-matrix(0,ncol=n,nrow=1)
						for(i in 1:n){#this level
							payoffs[i] <- r[slev,i] + r[slev+1,r2[slev+1]]
						}
						best <- which(payoffs == max(payoffs))
						r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
				}
			}
			else if(r2[slev+1]==0){
				#if we don't know the next level
				if(r2[slev-1]>0 & r2[slev-2]>0){
					#if we know both previous levels
					payoffs <- matrix(0,ncol=n,nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- penalties[[slev]][r2[slev-2],r2[slev-1],i] + penalties[[slev+1]][r2[slev-1],i,j]
						}
					}
					best <- which(payoffs == max(payoffs), arr.ind=T)
					r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
				else{
					#if we don't know both prev levels
					if(r2[slev-1]>0){
						#if we know previous level
						payoffs <- matrix(0, ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][r2[slev-1],i,j]
							}
						}
						best <- which(payoffs == max(payoffs), arr.ind=T)
						r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the previous level
						payoffs <- array(0,dim=c(n,n))
						for(i in 1:n){#trait for previous level
							for(j in 1:n){#this is the trait for this level
								payoffs[i,j] <- r[slev,i] + r[slev+1,j] #base payoff this levl + base payoff next lev
							}
						}
						best <- which(payoffs == max(payoffs), arr.ind=T)
						r2[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}					
				}
			}
		}
		else if(slev==s){
			#if on the last level
			if(r2[slev-1]>0 & r2[slev-2]>0){
				#if we know both previous levels
				payoffs <- matrix(0, ncol=n, nrow=1)
				for(i in 1:n){
					payoffs[i] <- penalties[[slev]][r2[slev-2],r2[slev-1],i]
				}
				best <- which(payoffs == max(payoffs))
				r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else{
				#if we don't know both prev traits
				best <- which(r[slev,] == max(r[slev,]))
				r2[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
		}
	}
	#calculate payoff
	tp <-r[1,r2[1]] + penalties[[2]][r2[1],r2[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][r2[i-2],r2[i-1], r2[i]]
	}
	ans <- list(r2, tp)
	return(ans)	
}

##
three_learner <- function(r,sdev){
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
		if(slev == 1){
			#if on the first level
			#we need to look ahead - if we know the second trait or third trait, we need to take that into account
			if(r3[slev+1]>0){
				#if we know the next trait
				if(r3[slev+2]>0){
					#if we know the 4th level
					if (r3[slev+1]>0){
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- r[1,i] + penalties[[2]][i,r3[slev+1]] + penalties[[3]][i,r3[slev+1],r3[slev+2]]
						}
						best <- which(payoffs == max(payoffs))
						r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					} 
				}
				else{
					#if we don't know the 4th level	
					payoffs <- matrix(0, ncol=n,nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- r[1,i] + penalties[[2]][i,r3[slev+1]] + penalties[[3]][i,r3[slev+1],j]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
			else{
				#if we don't know the next trait
				if(r3[slev+2]>0){
					#if we know the third trait
					payoffs <- matrix(0, ncol=n,nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][i,j,r3[slev+2]]
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
				else{
					#if we don't know the third trait
					payoffs <- array(0, dim=c(n,n,n))
					for(i in 1:n){
						for(j in 1:n){
							for(k in 1:n){
								payoffs[i,j,k] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][i,j,k]
							}
						}
					}
					best <- which(payoffs == max(payoffs),arr.ind=T)
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
			}
		}
		else if(slev == 2){
			if(r3[slev-1]>0){
				#if we know the previous level
				#we need to look ahead - if we know the second trait or third trait, we need to take that into account
				if(r3[slev+1]>0){
					#if we know the next trait
					if(r3[slev+2]>0){
						#if we know the third level
						if (r3[slev+1]>0){
							payoffs <- matrix(0,ncol=n,nrow=1)
							for(i in 1:n){
								payoffs[i] <- penalties[[2]][r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],r3[slev+2]]
							}
							best <- which(payoffs == max(payoffs))
							r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
						} 
					}
					else{
						#if we don't know the third level	
						payoffs <- matrix(0, ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- penalties[[2]][r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
				else{
					#if we don't know the next trait
					if(r3[slev+2]>0){
						#if we know the third trait
						payoffs <- matrix(0, ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- penalties[[2]][r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,j] + penalties[[slev+2]][i,j,r3[slev+2]]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the third trait
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){
							for(j in 1:n){
								for(k in 1:n){
									payoffs[i,j,k] <- penalties[[2]][r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,j] + penalties[[slev+2]][i,j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}
			else{
				#if we don't know the previous level
				#we need to look ahead 
				if(r3[slev+1]>0){
					#if we know the next trait
					if(r3[slev+2]>0){
						#if we know the 4th level
						if (r3[slev+1]>0){
							payoffs <- matrix(0,ncol=n,nrow=1)
							for(i in 1:n){#trait for previous level
								payoffs[i] <- r[slev,i] + r[slev+1,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],r3[slev+2]] #base payoff this lev+ base payoff next lev + penalties 3rd lev
							}
							best <- which(payoffs == max(payoffs))
							r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
						} 
					}
					else{
						#if we don't know the third level	
						payoffs <- array(0, dim=c(n,n))
						for(i in 1:n){#trait for this level
							for(j in 1:n){#trait for next level
									payoffs[i,j] <- r[slev,i] + r[slev+1,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
				else{
					#if we don't know the next trait
					if(r3[slev+2]>0){
						#if we know the 4th trait
						payoffs <- array(0, dim=c(n,n))
						for(i in 1:n){#this level
							for(j in 1:n){#next level
								payoffs[i,j] <- r[slev,i] + r[slev+1,j] + penalties[[slev+2]][i,j,r3[slev+2]]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),2], best[1,2])
					}
					else{
						#if we don't know the third trait
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){#this level
							for(j in 1:n){#next level
								for(k in 1:n){#third level
									payoffs[i,j,k] <- r[slev,i] + r[slev+1,j] + penalties[[slev+2]][i,j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}
		}
		else if(s-slev>=2){
			#if at least three levels left
			if(r3[slev-2]>0 & r3[slev-1]>0){
				#if we know both previous levels
				if(r3[slev+1]>0){
					#if we know the next trait
					if(r3[slev+2]>0){
						#if we know the next next level
						if (r3[slev+1]>0){
							payoffs <- matrix(0,ncol=n,nrow=1)
							for(i in 1:n){
								payoffs[i] <- penalties[[slev]][r3[slev-2],r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],r3[slev+2]]
							}
							best <- which(payoffs == max(payoffs))
							r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
						} 
					}
					else{
						#if we don't know the next next level	
						payoffs <- matrix(0, ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- penalties[[slev]][r3[slev-2],r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
				else{
					#if we don't know the next trait
					if(r3[slev+2]>0){
						#if we know the next next trait
						payoffs <- matrix(0, ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <-penalties[[slev]][r3[slev-2],r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,j] + penalties[[slev+2]][i,j,r3[slev+2]]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the next next trait
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){
							for(j in 1:n){
								for(k in 1:n){
									payoffs[i,j,k] <- penalties[[slev]][r3[slev-2],r3[slev-1],slev] + penalties[[slev+1]][r3[slev-1],i,j] + penalties[[slev+2]][i,j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}
			else if(r3[slev-1]>0){
				#if we know the previous level
				if(r3[slev+1]>0){
					#if we know the next trait
					if(r3[slev+2]>0){
						#if we know the 4th level
						if (r3[slev+1]>0){
							payoffs <- matrix(0,ncol=n,nrow=1)
							for(i in 1:n){
									payoffs[i] <- r[slev,i] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],r3[slev+2]]
							}
							best <- which(payoffs == max(payoffs), arr.ind=T)
							r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
						} 
					}
					else{
						#if we don't know the third level	
						payoffs <- matrix(0,ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
									payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],j]
								}
							}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
				else{
					#if we don't know the next trait
					if(r3[slev+2]>0){
						#if we know the 4th trait
						payoffs <- array(0, dim=c(n,n))
						for(i in 1:n){
							for(j in 1:n){
									payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][r3[slev-1],i,j] + penalties[[slev+2]][i,j,r3[slev+2]]
								}
							}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the third trait
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){
							for(j in 1:n){
								for(k in 1:n){
										payoffs[i,j,k] <- r[slev,i] + penalties[[slev+1]][r3[slev-1],i,j] + penalties[[slev+2]][i,j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}
			else if (r3[slev-1]==0){
				# if we don't know any of the two previous levels
				#we need to look ahead 
				if(r3[slev+1]>0){
					#if we know the next trait
					if(r3[slev+2]>0){
						#if we know the 4th level
						if (r3[slev+1]>0){
							payoffs <- matrix(0,ncol=n,nrow=1)
							for(i in 1:n){
								payoffs[i] <- r[slev,i] + r[slev+1,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],r3[slev+2]]
							}
							best <- which(payoffs == max(payoffs))
							r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
						} 
					}
					else{
						#if we don't know the third level	
						payoffs <- array(0, dim=c(n,n))
						for(i in 1:n){#trait for this level
							for(j in 1:n){#trait for third level
								payoffs[i,j] <- r[slev,i] + r[slev+1,r3[slev+1]] + penalties[[slev+2]][i,r3[slev+1],j]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
				else{
					#if we don't know the next trait
					if(r3[slev+2]>0){
						#if we know the 4th trait
						payoffs <- array(0, dim=c(n,n))
						for(i in 1:n){#this level
							for(j in 1:n){#next level
								payoffs[i,j] <- r[slev,i] + r[slev+1, j] + penalties[[slev+2]][i,j,r3[slev+2]]
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the third trait
						payoffs <- array(0, dim=c(n,n,n))
						for(i in 1:n){#this level
							for(j in 1:n){#next level
								for(k in 1:n){#third level
									payoffs[i,j,k] <- r[slev,i] + r[slev+1,j] + penalties[[slev+2]][i,j,k]
								}
							}
						}
						best <- which(payoffs == max(payoffs),arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
				}
			}				
		}
		else if(s-slev ==1){
			#if two levels left
			if(r3[slev+1]>0){
				#if we know the next trait
				if(r3[slev-1]>0 & r3[slev-2]>0){
					#if we know the previous two levels
					payoffs <- matrix(0,ncol=n,nrow=1)
					for(i in 1:n){
							payoffs[i] <- penalties[[slev]][r3[slev-2],r3[slev-1],i] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]]
						}
						best <- which(payoffs == max(payoffs))
						r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
				else{
					#if we don't know both the previous levels, learn from scratch (kind of?)
					if(r3[slev-1]>0){
						payoffs <- matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- r[slev,i] + penalties[[slev+1]][r3[slev-1],i,r3[slev+1]]
						}
						best <- which(payoffs == max(payoffs))
						r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
					else if(r3[slev-1]==0){
                        #if we don't know the previous level
						payoffs<-matrix(0,ncol=n,nrow=1)
						for(i in 1:n){
							payoffs[i] <- r[slev,i] + r[slev+1,r3[slev+1]] 
						}
						best <- which(payoffs == max(payoffs))
						r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
					}
				}
			}
			else if(r3[slev+1]==0){
				#if we don't know the next level
				if(r3[slev-1]>0 & r3[slev-2]>0){
					#if we know both previous levels
					payoffs <- matrix(0,ncol=n,nrow=n)
					for(i in 1:n){
						for(j in 1:n){
							payoffs[i,j] <- penalties[[slev]][r3[slev-2],r3[slev-1],i] + penalties[[slev+1]][r3[slev-1],i,j]
						}
					}
					best <- which(payoffs == max(payoffs), arr.ind=T)
					r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
				}
				else{
					#if we don't know both prev levels
					if(r3[slev-1]>0){
						#if we know previous level
						payoffs <- matrix(0, ncol=n,nrow=n)
						for(i in 1:n){
							for(j in 1:n){
								payoffs[i,j] <- r[slev,i] + penalties[[slev+1]][r3[slev-1],i,j]
							}
						}
						best <- which(payoffs == max(payoffs), arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}
					else{
						#if we don't know the previous level
						payoffs <- array(0,dim=c(n,n))
						for(i in 1:n){#trait for this level
							for(j in 1:n){#next level
								payoffs[i,j] <- r[slev,i] + r[slev+1,j]
							}
						}
						best <- which(payoffs == max(payoffs), arr.ind=T)
						r3[slev] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),1], best[1,1])
					}					
				}
			}
		}
		else if(slev==s){
			#if on the last level
			#if on the last level
			if(r3[slev-1]>0 & r3[slev-2]>0){
				#if we know both previous levels
				payoffs <- matrix(0, ncol=n, nrow=1)
				for(i in 1:n){
					payoffs[i] <- penalties[[slev]][r3[slev-2],r3[slev-1],i]
				}
				best <- which(payoffs == max(payoffs))
				r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
			else{
				#if we don't know both prev traits
				best <- which(r[slev,] == max(r[slev,]))
				r3[slev] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
			}
		}
	}
	tp <-r[1,r3[1]] + penalties[[2]][r3[1],r3[2]]
	# + penalties for each of the following levels, according to repertoire
	for (i in 3:s){
		tp <- tp + penalties[[i]][r3[i-2],r3[i-1], r3[i]]
	}
	ans <- list(r3, tp)
	return(ans)
}