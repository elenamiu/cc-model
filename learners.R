
one_learner <- function(r,sdev){
	sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	repertoire <- matrix(0,ncol=s,nrow=1) 
    best <- which(r[1,]==max(r[1,]))
	repertoire[,1] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	for(i in 2:s){
		dep <- dnorm(a,repertoire[i-1],sdev)/dnorm(repertoire[i-1],repertoire[i-1],sdev)
		penalty <- r[i,] * dep
		best <- which(penalty==max(penalty))
		repertoire[,i] <- ifelse(length(best)>1,best[sample(length(best),1)], best[1])
	}
	return(repertoire)
}

two_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
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
	return(r2)
}

#3-level learner
three_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# 3-level maximiser
	 r3 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s,3)){
		 if(s-lev >=3){
			 if(lev==1){
				 payoffs<-array(0,dim=c(n,n,n))
				 for (i in 1:n){
				     for (j in 1:n){
			    		 for (k in 1:n){
							 payoffs[i,j,k] <- r[lev,i] + penalties[[lev+1]][i,j] + penalties[[lev+2]][j,k]
				        }
				     }
				 }
				 best <- which(payoffs == max(payoffs), arr.ind=T)
				 #sample if there is more than one option
				 # row <- sample(1:nrow(best),1)
	 # 				 r3[lev:(lev+2)] <- best[row,]
				 r3[lev:(lev+2)] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),], best[1,])
			 }else{
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
				 # row <- sample(1:nrow(best),1)
				 r3[lev:(lev+2)] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),], best[1,])
			 }
		 }
		 else if(s-lev == 1) {
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- penalties[[lev]][r3[lev-1],i] + penalties[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r3[lev] <- best[1]
	        r3[lev+1] <- best[2]
		 }
		 else if(s==lev){
			 best <- which(penalties[[lev]][r3[lev-1],] == max(penalties[[lev]][r3[lev-1],]))
			 r3[lev] <-best
		 }
	 }
	return(r3)
}

#4-level learner
four_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# 4-level maximiser
	 r4 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s,4)){
		 if(s-lev >=4){
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
				 r4[lev:(lev+3)] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),], best[1,])
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
				 # row <- sample(1:nrow(best),1)
				 r4[lev:(lev+3)] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),], best[1,])
			 }
		 }
		 else if (s-lev == 2){
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
			 # row <- sample(1:nrow(best),1)
			 r3[lev:(lev+2)] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),], best[1,])
		 }
		 else if(s-lev == 1) {
			payoffs <- matrix(0, ncol= n, nrow=n)
	        for (i in 1:n){
 	            for(j in 1:n){
 	                payoffs[i,j] <- penalties[[lev]][r4[lev-1],i] + penalties[[lev+1]][i,j]  
 	            }
 	        }
	        best <- which(payoffs == max(payoffs), arr.ind=T)
	        r4[lev] <- best[1]
	        r4[lev+1] <- best[2]
		 }
		 else if(s==lev){
			 best <- which(penalties[[lev]][r4[lev-1],] == max(penalties[[lev]][r4[lev-1],]))
			 r4[lev] <-best
		 }
	 }
	return(r4)
}

## 5-level learner
five_learner <- function(r,sdev){
    sdev <- sdev
	n <- ncol(r)
	s <- nrow(r)
	a <- 1:n
	penalties<- list()
	for (level in c(2:s)){
		# r2 <- matrix(0, ncol=n, nrow=s-1)
		ps <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			dep <- dnorm(a,i,sdev)/dnorm(i,i,sdev)
			penalty <- r[level,] * dep
			ps[i,] <- penalty
		}
		penalties[[level]] <- ps
	}

	# 5-level maximiser
	 r5 <- matrix(0, ncol=s, nrow=1)
	 for (lev in seq(1,s,5)){
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
	 # 		 row <- sample(1:nrow(best),1)
	 # 		 r5[lev:(lev+4)] <- best[row,]
		 r5[lev:(lev+4)] <- ifelse(nrow(best)>1,best[sample(nrow(best),1),], best[1,])
	 }
	return(r5)
}