library(gridExtra)
#########
## figure out what makes a hard task
## compare 1 level to 5 level learner 

###########################################
# 1. NTRAITS

# PENALTY

#check how the payoff distribution changes with number of traits - same distributions??
d10 <- NULL
d100 <- NULL
d1000<-NULL
for(run in 1:1000){
	n <- 10
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	d10 <- c(d10, as.vector(r))
	n <- 100
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	d100 <- c(d100, as.vector(r))
	n <- 1000
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}
	d1000 <- c(d1000, as.vector(r))
}

# > max(d10)
# [1] 304
# > max(d100)
# [1] 364
# > max(d1000)
# [1] 525 

# > max(d10)
# [1] 289
# > max(d100)
# [1] 355
# > max(d1000)
# [1] 561  BUT THESE NUMBERS CHANGE A LOT

###################
### N = 10
##
#Do this 10 times 
rp <- list()
p <- list()
for(run in 1:100){
	n <- 10
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp[[run]] <- rbind(repertoire, r2)
	p[[run]] <- c(tp, tp2)
}

## how similar
sim<-rep(0,length(rp))
pdiff<- rep(0,length(rp))
for(run in 1:length(rp)){
	rpr <- rp[[run]]
	sim[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff[run] <- p[[run]][2] - p[[run]][1]
}

##
#### N = 100
rp100 <- list()
p100 <- list()
for(run in 1:100){
	print(run)
	n <- 100
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# r5 <- matrix(0, ncol=s, nrow=1)
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,k] + penalties[[4]][k,l] + penalties[[5]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# #sample if there is more than one option
	# row <- sample(1:nrow(best),1)
	# r5[1:5] <- best[row,]
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- penalties[[6]][r5[5],i] + penalties[[7]][i,j] + penalties[[8]][j,k] + penalties[[9]][k,l] + penalties[[10]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# row <- sample(1:nrow(best),1)
	# r5[6:10] <- best[row,]
	
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp100[[run]] <- rbind(repertoire, r2)
	p100[[run]] <- c(tp, tp2)
}

sim100<-rep(0,length(rp100))
pdiff100<- rep(0,length(rp100))
for(run in 1:length(rp100)){
	rpr <- rp100[[run]]
	sim100[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff100[run] <- p100[[run]][2] - p100[[run]][1]
}

##################
## N = 1000
##
rp1000 <- list()
p1000 <- list()
for(run in 1:100){
	print(run)
	n <- 1000
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# r5 <- matrix(0, ncol=s, nrow=1)
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,k] + penalties[[4]][k,l] + penalties[[5]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# #sample if there is more than one option
	# row <- sample(1:nrow(best),1)
	# r5[1:5] <- best[row,]
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- penalties[[6]][r5[5],i] + penalties[[7]][i,j] + penalties[[8]][j,k] + penalties[[9]][k,l] + penalties[[10]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# row <- sample(1:nrow(best),1)
	# r5[6:10] <- best[row,]
	
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp1000[[run]] <- rbind(repertoire, r2)
	p1000[[run]] <- c(tp, tp2)
}

sim1000<-rep(0,length(rp1000))
pdiff1000<- rep(0,length(rp1000))
for(run in 1:length(rp1000)){
	rpr <- rp1000[[run]]
	sim1000[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff1000[run] <- p1000[[run]][2] - p1000[[run]][1]
}

# INCREMENT

###########################################
# 2. S-LEVELS
# PENALTY
#10 levels
rp <- list()
p <- list()
for(run in 1:200){
	n <- 10
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 2-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}
	
	###
	# 5 level	
	 r5 <- matrix(0, ncol=s, nrow=1)
	 payoffs<-array(0,dim=c(n,n,n,n,n))
	 for (i in 1:n){
	     for (j in 1:n){
	         for (k in 1:n){
	             for(l in 1:n){
	                 for(m in 1:n){
	                     payoffs[i,j,k,l,m] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,k] + penalties[[4]][k,l] + penalties[[5]][l,m]
	                 }
	             }
	         }
	     }
	 }
	 best <- which(payoffs == max(payoffs), arr.ind=T)
	 #sample if there is more than one option
	 row <- sample(1:nrow(best),1)
	 r5[1:5] <- best[row,]
	 payoffs<-array(0,dim=c(n,n,n,n,n))
	 for (i in 1:n){
	     for (j in 1:n){
	         for (k in 1:n){
	             for(l in 1:n){
	                 for(m in 1:n){
	                     payoffs[i,j,k,l,m] <- penalties[[6]][r5[5],i] + penalties[[7]][i,j] + penalties[[8]][j,k] + penalties[[9]][k,l] + penalties[[10]][l,m]
	                 }
	             }
	         }
	     }
	 }
	 best <- which(payoffs == max(payoffs), arr.ind=T)
	 row <- sample(1:nrow(best),1)
	 r5[6:10] <- best[row,]

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	tp5 <-r[1,r2[1]]
	for (i in 2:s){
		tp5 <- tp5 + penalties[[i]][r5[i-1], r5[i]]
	}
	rp[[run]] <- rbind(repertoire, r2, r5)
	p[[run]] <- c(tp, tp2, tp5)
}

## how similar
sim<-rep(0,length(rp))
pdiff<- rep(0,length(rp))
for(run in 1:length(rp)){
	rpr <- rp[[run]]
	sim[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff[run] <- p[[run]][2] - p[[run]][1]
}

####################
## check improvement 
p21 <- unlist(lapply(p, '[[',2)) - unlist(lapply(p, '[[',1))
p32 <- unlist(lapply(p, '[[',3)) - unlist(lapply(p, '[[',2))

p1 <- qplot(p21)+ geom_histogram(binwidth=10)
p2 <- qplot(p32) + geom_histogram(binwidth=10)

 # tiff("~/Google Drive/ASU/mesoudi_model/hard/learner_payoff_diff_10_v200runs.tiff")
 # grid.arrange(p1,p2)
 # dev.off()

##
#### s = 100
rp100 <- list()
p100 <- list()
for(run in 1:200){
	print(run)
	n <- 10
	s <- 100
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	 r5 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s,5)){
		 payoffs<-array(0,dim=c(n,n,n,n,n))
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
		 best <- which(payoffs == max(payoffs), arr.ind=T)
		 #sample if there is more than one option
		 row <- sample(1:nrow(best),1)
		 r5[lev:(lev+4)] <- best[row,]
	 }
	
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	tp5 <-r[1,r2[1]]
	for (i in 2:s){
		tp5 <- tp5 + penalties[[i]][r5[i-1], r5[i]]
	}
	rp100[[run]] <- rbind(repertoire, r2, r5)
	p100[[run]] <- c(tp, tp2, tp5)
}

sim100<-rep(0,length(rp100))
pdiff100<- rep(0,length(rp100))
for(run in 1:length(rp100)){
	rpr <- rp100[[run]]
	sim100[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff100[run] <- p100[[run]][2] - p100[[run]][1]
}


####################
## check improvement 
p21 <- unlist(lapply(p100, '[[',2)) - unlist(lapply(p100, '[[',1))
p32 <- unlist(lapply(p100, '[[',3)) - unlist(lapply(p100, '[[',2))

p1 <- qplot(p21)+ geom_histogram(binwidth=10)
p2 <- qplot(p32) + geom_histogram(binwidth=10)

 # tiff("~/Google Drive/ASU/mesoudi_model/hard/learner_payoff_diff_s100_v100runs.tiff")
 grid.arrange(p1,p2)
 dev.off()
## S=1000
rp1000 <- list()
p1000 <- list()
for(run in 1:200){
	print(run)
	n <- 10
	s <- 1000
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# r5 <- matrix(0, ncol=s, nrow=1)
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,k] + penalties[[4]][k,l] + penalties[[5]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# #sample if there is more than one option
	# row <- sample(1:nrow(best),1)
	# r5[1:5] <- best[row,]
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- penalties[[6]][r5[5],i] + penalties[[7]][i,j] + penalties[[8]][j,k] + penalties[[9]][k,l] + penalties[[10]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# row <- sample(1:nrow(best),1)
	# r5[6:10] <- best[row,]
	
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp1000[[run]] <- rbind(repertoire, r2)
	p1000[[run]] <- c(tp, tp2)
}

sim1000<-rep(0,length(rp1000))
pdiff1000<- rep(0,length(rp1000))
for(run in 1:length(rp1000)){
	rpr <- rp1000[[run]]
	sim1000[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff1000[run] <- p1000[[run]][2] - p1000[[run]][1]
}

p1 <- qplot(pdiff/10) + geom_histogram()
p2 <- qplot(pdiff100/100) + geom_histogram()
p3 <- qplot(pdiff1000/1000) + geom_histogram()
 # tiff("/Users/elena/Google Drive/ASU/mesoudi_model/hard/pdiff_slevel_v200.tiff")
 # grid.arrange(p1,p2,p3)
 # dev.off()

#########
########
# ALSO S LEVEL
# N=100
# PENALTY
#10 levels
rp <- list()
p <- list()
for(run in 1:100){
	n <- 100
	s <- 10
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 2-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}
	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp[[run]] <- rbind(repertoire, r2)
	p[[run]] <- c(tp, tp2)
}

## how similar
sim<-rep(0,length(rp))
pdiff<- rep(0,length(rp))
for(run in 1:length(rp)){
	rpr <- rp[[run]]
	sim[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff[run] <- p[[run]][2] - p[[run]][1]
}

##
#### s = 100
rp100 <- list()
p100 <- list()
for(run in 1:100){
	print(run)
	n <- 100
	s <- 100
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp100[[run]] <- rbind(repertoire, r2)
	p100[[run]] <- c(tp, tp2)
}

sim100<-rep(0,length(rp100))
pdiff100<- rep(0,length(rp100))
for(run in 1:length(rp100)){
	rpr <- rp100[[run]]
	sim100[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff100[run] <- p100[[run]][2] - p100[[run]][1]
}


## S=1000
rp1000 <- list()
p1000 <- list()
for(run in 1:100){
	print(run)
	n <- 100
	s <- 1000
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}

	## 5-level learner
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# r5 <- matrix(0, ncol=s, nrow=1)
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- r[1,i] + penalties[[2]][i,j] + penalties[[3]][j,k] + penalties[[4]][k,l] + penalties[[5]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# #sample if there is more than one option
	# row <- sample(1:nrow(best),1)
	# r5[1:5] <- best[row,]
	# payoffs<-array(0,dim=c(n,n,n,n,n))
	# for (i in 1:n){
	#     for (j in 1:n){
	#         for (k in 1:n){
	#             for(l in 1:n){
	#                 for(m in 1:n){
	#                     payoffs[i,j,k,l,m] <- penalties[[6]][r5[5],i] + penalties[[7]][i,j] + penalties[[8]][j,k] + penalties[[9]][k,l] + penalties[[10]][l,m]
	#                 }
	#             }
	#         }
	#     }
	# }
	# best <- which(payoffs == max(payoffs), arr.ind=T)
	# row <- sample(1:nrow(best),1)
	# r5[6:10] <- best[row,]
	
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp1000[[run]] <- rbind(repertoire, r2)
	p1000[[run]] <- c(tp, tp2)
}

sim1000<-rep(0,length(rp1000))
pdiff1000<- rep(0,length(rp1000))
for(run in 1:length(rp1000)){
	rpr <- rp1000[[run]]
	sim1000[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff1000[run] <- p1000[[run]][2] - p1000[[run]][1]
}

p1 <- qplot(pdiff/10) + geom_histogram()
p2 <- qplot(pdiff100/100) + geom_histogram()
p3 <- qplot(pdiff1000/1000) + geom_histogram()
 tiff("/Users/elena/Google Drive/ASU/mesoudi_model/hard/pdiff_slevel_n100.tiff")
 grid.arrange(p1,p2,p3)
  dev.off()

# INCREMENT
#########################################################################################
#########################################################################################
##
# 3. VARY SD
# PENALTY

#SD=1.5
rp100 <- list()
p100 <- list()
for(run in 1:500){
	print(run)
	n <- 100
	s <- 100
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}
	#TWO LEVEL LEARNER
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp100[[run]] <- rbind(repertoire, r2)
	p100[[run]] <- c(tp, tp2)
}

sim100<-rep(0,length(rp100))
pdiff100<- rep(0,length(rp100))
for(run in 1:length(rp100)){
	rpr <- rp100[[run]]
	sim100[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff100[run] <- p100[[run]][2] - p100[[run]][1]
}

## SD=10

rp10 <- list()
p10 <- list()
for(run in 1:500){
	print(run)
	n <- 100
	s <- 100
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],10)/dnorm(repertoire[i-1],repertoire[i-1],10)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}
	#TWO LEVEL LEARNER
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,10)/dnorm(i,i,10)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp10[[run]] <- rbind(repertoire, r2)
	p10[[run]] <- c(tp, tp2)
}

sim10<-rep(0,length(rp10))
pdiff10<- rep(0,length(rp10))
for(run in 1:length(rp10)){
	rpr <- rp10[[run]]
	sim10[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff10[run] <- p10[[run]][2] - p10[[run]][1]
}

#######
## SD = 20

rp20 <- list()
p20 <- list()
for(run in 1:500){
	print(run)
	n <- 100
	s <- 100
	r <- matrix(0, ncol=n, nrow=s)
	#assign payoffs
	for (i in 1:s){
		r[i,] <- rexp(n, rate=1)
		r[i,] <- round(2*(r[i,]^2))
	}

	#ONE LEVEL MAXIMISER learner
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
	repertoire[,1] <- sample(which(r[1,] == max(r[1,])),1)

	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],20)/dnorm(repertoire[i-1],repertoire[i-1],20)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- sample(best,1)
	}
	#TWO LEVEL LEARNER
	a <- 1:n
	#penalties[[s]][i][j] - adjusted payoffs for each trait j, if trait i were chosen at level s-1
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,20)/dnorm(i,i,20)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}
	r2 <- matrix(0, ncol=s, nrow=1)
	#first level
	payoffs <- matrix(0, ncol=n, nrow=n)
	for (i in 1:n){
	    for (j in 1:n){
	        payoffs[i,j] <- r[1,i] + penalties[[2]][i,j]    
	    }
	}
	best <- which(payoffs == max(payoffs),arr.ind=T)
	r2[1] <- best[1]
	r2[2] <- best[2]
	for (level in seq(3,s,2)){
	    if(level != s){
	        payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
	            for(j in 1:n){
	                payoffs[i,j] <- penalties[[level]][r2[level-1],i] + penalties[[level+1]][i,j]  
	            }
	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r2[level] <- best[1]
	        r2[level+1] <- best[2]
	    } else{
	        payoffs <- penalties[[level]][r2[level-1],]
	        r2[level] <- which(payoffs == max(payoffs))
	    }
	}	

	#compare paypoffs
	tp <-r[1,repertoire[1]]
	for (i in 2:s){
		tp <- tp + penalties[[i]][repertoire[i-1], repertoire[i]]
	}
	tp2 <-r[1,r2[1]]
	for (i in 2:s){
		tp2 <- tp2 + penalties[[i]][r2[i-1], r2[i]]
	}
	rp20[[run]] <- rbind(repertoire, r2)
	p20[[run]] <- c(tp, tp2)
}

sim20<-rep(0,length(rp20))
pdiff20<- rep(0,length(rp20))
for(run in 1:length(rp20)){
	rpr <- rp20[[run]]
	sim20[run] <- length(intersect(rpr[1,], rpr[2,]))/length(rpr)
	pdiff20[run] <- p20[[run]][2] - p20[[run]][1]
}
##
#differences?
p1 <- qplot(pdiff100) + geom_histogram() + xlab("pdiff (sd=1.5)")
p2 <- qplot(pdiff10) + geom_histogram() + xlab("pdiff (sd=10)")
p3 <- qplot(pdiff20) + geom_histogram() + xlab("pdiff (sd=20)")
 tiff("~/Google Drive/ASU/mesoudi_model/hard/sd_pdiff_200runs.tiff")
 grid.arrange(p1,p2,p3)
 dev.off()

p1 <- qplot(sim100) + geom_histogram() + xlab("sim (sd=1.5)")
p2 <- qplot(sim10) + geom_histogram() + xlab("sim (sd=10)")
p3 <- qplot(sim20) + geom_histogram() + xlab("sim (sd=20)")
tiff("~/Google Drive/ASU/mesoudi_model/hard/sd_sim_200runs.tiff")
grid.arrange(p1,p2,p3)
dev.off()

# INCREMENT
###########################################
# 4. PENALTY VS INCREMENT