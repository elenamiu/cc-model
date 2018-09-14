library(ggplot2)
library(gridExtra)
library(iterpc)

n<-100
nreward <- rexp(n, rate=1)
nreward <- round(2*(nreward^2))

nreward2 <- rexp(n, rate=1)
nreward2 <- round(2*(nreward2^2))
# plot(nreward2,type="l")

# plot(dnorm(a,0,5)/dnorm(0,0,5))
#

#############
p1 <- qplot(c(1:length(nreward)),nreward) + 
	geom_line() +
	xlab("traits") + 
	ylab("level 1 payoff")
trait1 <- 81
p2 <- qplot(c(1:length(nreward2)),nreward2,alpha=0.1) + 
	geom_line(alpha=0.3) +
	theme(legend.position="none") +
	xlab("traits") +
	ylab("level 2 payoff")
a <- c(1:n)

increment1 <-dnorm(a,trait1,10)/dnorm(trait1,trait1,10) * mean(nreward2)
increment2 <-dnorm(a,trait1,10)/dnorm(trait1,trait1,10) * nreward[trait1]



p3 <- p2 + geom_line(aes(1:n, increment1), col="tomato")
p4 <- p2 + geom_line(aes(1:n, increment2), col="tomato")

grid.arrange(p1,p3,p4)


#######################
# calculate increment
# dep <- dnorm(a,trait1,20)/dnorm(trait1,trait1,20)
 dep <- dnorm(a,trait1,10)/dnorm(trait1,trait1,10)
#plot increment
p3 <- p2 + geom_line(aes(1:n, dep), col="tomato",alpha=1) +
	ylab("dependency")
#incorporate reward2 and penalty
penalty <- nreward2 * dep
p4 <- p2 + geom_line(aes(1:n, penalty), col="darkturquoise", alpha=1) +
	ylab("penalty")
p5 <- p4 + geom_line(aes(1:n,nreward[trait1]+penalty), col="mediumpurple",alpha=1)	

#incorporate increment
increment <- nreward2 + (dep * nreward[trait1])/2 
p6 <- p2 + geom_line(aes(1:n, increment), col="darkturquoise", alpha=1) +
	ylab("increment")
	
p7 <- p6 + geom_line(aes(1:n,nreward[trait1]+increment), col="mediumpurple", alpha=1)

# tiff("/Users/elena/Google Drive/ASU/mesoudi_model/deps/deps2.tiff",height=20, width = 15, res=300, units="cm")
grid.arrange(p1,p3,p4,p5,p6,p7,ncol=1)
dev.off()

