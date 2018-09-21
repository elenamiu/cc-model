#############################################
## PLOT DEPENDENCIES
library(ggplot2)
library(gridExtra)


######## TWO STEP DEPENDENCY

n<-100
nreward <- rexp(n, rate=1)
nreward <- round(2*(nreward^2))

nreward2 <- rexp(n, rate=1)
nreward2 <- round(2*(nreward2^2))

#############
p1 <- qplot(c(1:length(nreward)),nreward) + 
	geom_line() +
	xlab("traits") + 
	ylab("level 1 payoff") +
	geom_point(aes(x=trait1,y=nreward[trait1]), col="tomato", size=3) 
# trait1 <- 27


p2 <- qplot(c(1:length(nreward2)),nreward2,alpha=0.1) + 
	geom_line(alpha=0.3) +
	theme(legend.position="none") +
	xlab("traits") +
	ylab("level 2 payoff")
a <- c(1:n)

#######################
# calculate increment
dep <- dnorm(a,trait1,10)/dnorm(trait1,trait1,10)
#plot increment
p3 <- p2 + geom_line(aes(1:n, dep), col="tomato",alpha=1) +
	ylab("dependency")
#incorporate reward2 and penalty
penalty <- nreward2 * dep
p4 <- p2 + geom_line(aes(1:n, penalty), col="darkturquoise", alpha=1) +
	ylab("penalised second level payoffs")


png("/Users/elena/Google Drive/ASU/mesoudi_model/pres_pics/pics/deps_2lev.tiff",height=20, width = 18, res=300, units="cm")
grid.arrange(p1,p3,p4,ncol=1)
dev.off()


#################################################################
## THREE LEVEL DEPENDENCIES
n<-100
nreward <- rexp(n, rate=1)
nreward <- round(2*(nreward^2))

nreward2 <- rexp(n, rate=1)
nreward2 <- round(2*(nreward2^2))

nreward3 <- rexp(n, rate=1)
nreward3 <- round(2*(nreward3^2))

trait1 <- 53
trait2 <- 26 

#level 1 rewards
p1 <- qplot(c(1:length(nreward)),nreward) + 
	geom_line() +
	geom_point(aes(x=trait1,y=nreward[trait1]), col="tomato", size=3) +
	xlab("traits") + 
	ylab("level 1 payoff")
#level 2 rewards
p2 <- qplot(c(1:length(nreward2)),nreward2,alpha=0.1) + 
	geom_line(alpha=0.3) +
	theme(legend.position="none") +
	geom_point(aes(x=trait2,y=nreward2[trait2]), col="tomato", size=3) +
	xlab("traits") +
	ylab("level 2 payoff")
#level 3 rewards
p3 <- qplot(c(1:length(nreward3)),nreward3,alpha=0.1) + 
	geom_line(alpha=0.3) +
	theme(legend.position="none") +
	xlab("traits") +
	ylab("level 3 payoff")

#######################
# calculate penalties
a <- c(1:n)
dep <- dnorm(a,trait1,10)/dnorm(trait1,trait1,10)
#plot window
# p22 <- p2 + geom_line(aes(1:n, dep), col="tomato",alpha=1) +
	ylab("dependency")
#plot penalised payoffs
penalty <- nreward2 * dep
p22 <- p2 + geom_line(aes(1:n, penalty), col="darkturquoise", alpha=1) +
	ylab("penalised 2nd level payoffs")

dep2 <- dnorm(a,trait2,10)/dnorm(trait2,trait2,10)
penalty2 <- nreward3 * dep * dep2

p32 <- p3 + 
	geom_line(aes(1:n,nreward3*dep2), col = "purple")+
	geom_line(aes(1:n, penalty2), col="darkturquoise", alpha=1) +
	ylab("penalised 3rd level payoffs")

png("/Users/elena/Google Drive/ASU/mesoudi_model/pres_pics/pics/deps_2l3ev.tiff",height=20, width = 18, res=300, units="cm")	
grid.arrange(p1,p22,p32)
dev.off()