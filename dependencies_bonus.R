################
## ADDED INCREMENT DPENDENCIES
# payoff = base payoff + bonus reward from previous level, halved
library(ggplot2)
library(gridExtra)

##########
#
n <- 10
# s <- 5
s <- 10
r <- matrix(0, ncol=n, nrow=s)
#assign payoffs
for (i in 1:s){
	r[i,] <- rexp(n, rate=1)
	r[i,] <- round(2*(r[i,]^2))
}

##
# ONE LEVEL MAXIMISER
# dep <- dnorm(a,trait1,10)/dnorm(trait1,trait1,10)
# increment <- nreward2 + (dep * nreward[trait1])/2 

a <- 1:n
repertoire <- matrix(0, ncol=s, nrow=1)
repertoire[1] <- which(r[1,] == max(r[1,]))
for(i in 2:s){
	dep <- dnorm(a,repertoire[i-1],1.5)/dnorm(repertoire[i-1],repertoire[i-1],1.5)
	increment <- r[i,] + (dep * r[i-1,repertoire[i-1]])/2
	repertoire[i] <- which(increment == max(increment))
}

##
# total payoff
tp <-0
for (i in 1:s){
	tp <- tp + r[i, repertoire[i]]
}


##########
## PAIRWISE COMBINATIONS
a<-1:n
#incs[[s]][i,j] - adjusted payoffs for each trait j at level s, if trait i were chosen at the previous level
incs <- list()
for(level in 2:s){
	ps<- matrix(0,ncol=n, nrow=n)
	for(i in 1:n){
		dep <- dnorm(a,i,1.5)/dnorm(i,i,1.5)
		inc <- r[level,] + (dep*r[level-1,i])/2
		ps[i,] <- inc
	}
	incs[[level]] <-ps
}
#############
## TWO LEVEL LEARNER
r2 <- matrix(0, ncol=s, nrow=1)
#first level
payoffs <- matrix(0,ncol=n, nrow=n)
#payoffs[i,j] - payoff when trait chosen at level 1 is i and trait at level 2 is j
for(i in 1:n){
	for(j in 1:n){
		payoffs[i,j] <- r[1,i] + incs[[2]][i,j]
	}
}
best <- which(payoffs ==max(payoffs), arr.ind=T)
r2[1] <- best[1]
r2[2] <- best[2]

for(level in seq(3,s,2)){
	if(level!=s){
		#best traits for s and s+1, based on what was chosen at s-1
		payoffs <- matrix(0,ncol=n,nrow=n)
		for (i in 1:n){
			for(j in 1:n){
				#increased payoff acc to previous level for "level" + increased payoff for "level"+1, if i were chosen
				payoffs[i,j] <- incs[[level]][r2[level-1],i] + incs[[level+1]][i,j]
			}
		}
		best <- which(payoffs == max(payoffs), arr.ind=T)
		r2[level] <- best[1]
		r2[level+1] <- best[2]
	} else{
		payoffs <- incs[[level]][r2[level-1],]
		r2[level] <- which(payoffs==max(payoffs))
	}
}

#payoff 
tp2 <-0
for (i in 1:s){
	tp2 <- tp2 + r[i, r2[i]]
}


##############
## 5 LEVELS
r5 <- matrix(0, ncol=s, nrow=1)
payoffs<-array(0,dim=c(n,n,n,n,n))
for (i in 1:n){
    for (j in 1:n){
        for (k in 1:n){
            for(l in 1:n){
                for(m in 1:n){
                    payoffs[i,j,k,l,m] <- r[1,i] + incs[[2]][i,j] + incs[[3]][j,k] + incs[[4]][k,l] + incs[[5]][l,m]         
                }
            }
        }
    }
}
best <- which(payoffs == max(payoffs), arr.ind=T)
r5[1:5] <- best
payoffs<-array(0,dim=c(n,n,n,n,n))
for (i in 1:n){
    for (j in 1:n){
        for (k in 1:n){
            for(l in 1:n){
                for(m in 1:n){
                    payoffs[i,j,k,l,m] <- incs[[6]][r5[5],i] + incs[[7]][i,j] + incs[[8]][j,k] + incs[[9]][k,l] + incs[[10]][l,m]         
                }
            }
        }
    }
}
best <- which(payoffs == max(payoffs), arr.ind=T)
r5[6:10] <- best

#payoff 
tp5 <-0
for (i in 1:s){
	tp5 <- tp5 + r[i, r5[i]]
}
#########################################
## PLOTS
##
##############################
## one level
data<- data.frame(c(1:10))
data <- cbind(data,(r[1,]))
colnames(data) <- c("traits","payoff")
p1 <- ggplot(data) +
	geom_line(aes(x=traits, y=payoff)) +
	geom_point(aes(x=traits, y=payoff)) +
	# geom_point(aes(x=repertoire[1],y=r[1,repertoire[1]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[1],y=payoff[repertoire[1]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[1],1.5)/dnorm(repertoire[1],repertoire[1],1.5)
penalty1 <- r[2,] * dep
p2 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[2,])) +
	geom_point(aes(x=data$traits, y=r[2,])) +
	geom_line(aes(x=data$traits, y=penalty1), col="darkturquoise") +
	# geom_point(aes(x=repertoire[2],y=r[2,repertoire[2]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[2],y=penalty1[repertoire[2]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[2],1.5)/dnorm(repertoire[2],repertoire[2],1.5)
penalty2 <- r[3,] * dep
p3 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[3,])) +
	geom_point(aes(x=data$traits, y=r[3,]))+
	geom_line(aes(x=data$traits, y=penalty2), col="darkturquoise") +
	# geom_point(aes(x=repertoire[3],y=r[3,repertoire[3]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[3],y=penalty2[repertoire[3]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[3],1.5)/dnorm(repertoire[3],repertoire[3],1.5)
penalty3 <- r[4,] * dep
p4 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[4,])) +
	geom_point(aes(x=data$traits, y=r[4,]))+
	geom_line(aes(x=data$traits, y=penalty3), col="darkturquoise") +
	# geom_point(aes(x=repertoire[4],y=r[4,repertoire[4]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[4],y=penalty3[repertoire[4]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[4],1.5)/dnorm(repertoire[4],repertoire[4],1.5)
penalty4 <- r[5,] * dep
p5 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[5,])) +
	geom_point(aes(x=data$traits, y=r[5,]))+
	geom_line(aes(x=data$traits, y=penalty4), col="darkturquoise") +
	# geom_point(aes(x=repertoire[5],y=r[5,repertoire[5]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[5],y=penalty4[repertoire[5]]),col="tomato",size=3)
    #
dep <- dnorm(a,repertoire[5],1.5)/dnorm(repertoire[5],repertoire[5],1.5)
penalty5 <- r[6,] * dep
p6 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[6,])) +
	geom_point(aes(x=data$traits, y=r[6,]))+
	geom_line(aes(x=data$traits, y=penalty5), col="darkturquoise") +
	# geom_point(aes(x=repertoire[6],y=r[6,repertoire[5]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[6],y=penalty5[repertoire[6]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[6],1.5)/dnorm(repertoire[6],repertoire[6],1.5)
penalty6 <- r[7,] * dep
p7 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[7,])) +
	geom_point(aes(x=data$traits, y=r[7,]))+
	geom_line(aes(x=data$traits, y=penalty6), col="darkturquoise") +
	# geom_point(aes(x=repertoire[7],y=r[7,repertoire[7]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[7],y=penalty6[repertoire[7]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[7],1.5)/dnorm(repertoire[7],repertoire[7],1.5)
penalty7 <- r[8,] * dep
p8 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[8,])) +
	geom_point(aes(x=data$traits, y=r[8,]))+
	geom_line(aes(x=data$traits, y=penalty7), col="darkturquoise") +
	# geom_point(aes(x=repertoire[8],y=r[8,repertoire[8]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[8],y=penalty7[repertoire[8]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[8],1.5)/dnorm(repertoire[8],repertoire[8],1.5)
penalty8 <- r[9,] * dep
p9 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[9,])) +
	geom_point(aes(x=data$traits, y=r[9,]))+
	geom_line(aes(x=data$traits, y=penalty8), col="darkturquoise") +
	# geom_point(aes(x=repertoire[9],y=r[9,repertoire[9]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[9],y=penalty8[repertoire[9]]),col="tomato",size=3)
#
dep <- dnorm(a,repertoire[9],1.5)/dnorm(repertoire[9],repertoire[9],1.5)
penalty9 <- r[10,] * dep
p10 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[10,])) +
	geom_point(aes(x=data$traits, y=r[10,]))+
	geom_line(aes(x=data$traits, y=penalty9), col="darkturquoise") +
	# geom_point(aes(x=repertoire[10],y=r[10,repertoire[10]]),col="tomato",size=3)
	geom_point(aes(x=repertoire[10],y=penalty9[repertoire[10]]),col="tomato",size=3)

tiff("~/Google Drive/ASU/mesoudi_model/deps/10traits_inc.tiff",height=20, width = 15, res=300, units="cm")
p <- grid.arrange(p1, p6, p2, p7, p3, p8, p4, p9, p5, p10 ,ncol=2)
dev.off()


#############################
# 5 levels
data<- data.frame(c(1:10))
data <- cbind(data,(r[1,]))
colnames(data) <- c("traits","payoff")
p1 <- ggplot(data) +
	geom_line(aes(x=traits, y=payoff)) +
	geom_point(aes(x=traits, y=payoff)) +
	geom_point(aes(x=r5[1],y=payoff[r5[1]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[1],1.5)/dnorm(r5[1],r5[1],1.5)
penalty1 <- r[2,] * dep
p2 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[2,])) +
	geom_point(aes(x=data$traits, y=r[2,])) +
	geom_line(aes(x=data$traits, y=penalty1), col="darkturquoise") +
	geom_point(aes(x=r5[2],y=penalty1[r5[2]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[2],1.5)/dnorm(r5[2],r5[2],1.5)
penalty2 <- r[3,] * dep
p3 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[3,])) +
	geom_point(aes(x=data$traits, y=r[3,]))+
	geom_line(aes(x=data$traits, y=penalty2), col="darkturquoise") +
	geom_point(aes(x=r5[3],y=penalty2[r5[3]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[3],1.5)/dnorm(r5[3],r5[3],1.5)
penalty3 <- r[4,] * dep
p4 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[4,])) +
	geom_point(aes(x=data$traits, y=r[4,]))+
	geom_line(aes(x=data$traits, y=penalty3), col="darkturquoise") +
	geom_point(aes(x=r5[4],y=penalty3[r5[4]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[4],1.5)/dnorm(r5[4],r5[4],1.5)
penalty4 <- r[5,] * dep
p5 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[5,])) +
	geom_point(aes(x=data$traits, y=r[5,]))+
	geom_line(aes(x=data$traits, y=penalty4), col="darkturquoise") +
	geom_point(aes(x=r5[5],y=penalty4[r5[5]]),col="tomato",size=3)
    #
dep <- dnorm(a,r5[5],1.5)/dnorm(r5[5],r5[5],1.5)
penalty5 <- r[6,] * dep
p6 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[6,])) +
	geom_point(aes(x=data$traits, y=r[6,]))+
	geom_line(aes(x=data$traits, y=penalty5), col="darkturquoise") +
	geom_point(aes(x=r5[6],y=penalty5[r5[6]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[6],1.5)/dnorm(r5[6],r5[6],1.5)
penalty6 <- r[7,] * dep
p7 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[7,])) +
	geom_point(aes(x=data$traits, y=r[7,]))+
	geom_line(aes(x=data$traits, y=penalty6), col="darkturquoise") +
	geom_point(aes(x=r5[7],y=penalty6[r5[7]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[7],1.5)/dnorm(r5[7],r5[7],1.5)
penalty7 <- r[8,] * dep
p8 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[8,])) +
	geom_point(aes(x=data$traits, y=r[8,]))+
	geom_line(aes(x=data$traits, y=penalty7), col="darkturquoise") +
	geom_point(aes(x=r5[8],y=penalty7[r5[8]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[8],1.5)/dnorm(r5[8],r5[8],1.5)
penalty8 <- r[9,] * dep
p9 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[9,])) +
	geom_point(aes(x=data$traits, y=r[9,]))+
	geom_line(aes(x=data$traits, y=penalty8), col="darkturquoise") +
	geom_point(aes(x=r5[9],y=penalty8[r5[9]]),col="tomato",size=3)
#
dep <- dnorm(a,r5[9],1.5)/dnorm(r5[9],r5[9],1.5)
penalty9 <- r[10,] * dep
p10 <-ggplot(data) +
	geom_line(aes(x=data$traits, y=r[10,])) +
	geom_point(aes(x=data$traits, y=r[10,]))+
	geom_line(aes(x=data$traits, y=penalty9), col="darkturquoise") +
	geom_point(aes(x=r5[10],y=penalty9[r5[10]]),col="tomato",size=3)

tiff("~/Google Drive/ASU/mesoudi_model/deps/10traits_5dep_incs.tiff",height=20, width = 15, res=300, units="cm")
# p <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=2)
p <- grid.arrange(p1, p6, p2, p7, p3, p8, p4, p9, p5, p10 ,ncol=2)
dev.off()


