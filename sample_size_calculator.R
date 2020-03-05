#need this package for plotting (may need to install first)
library(ggplot2)

#settings:
iterations = 100 #number of subsamples at each sample size
popn=1000 #population size
low=3 #min sample size
high=1000 #max sample size
amean=20 #mean for sample a. if same as b, interpret significant outputs as type I error
bmean=20 #mean for sample b
sda=5 #standard deviation for a
sdb=5 #standard deviation for b

#run simulations
output <- matrix(ncol=1, nrow=iterations)
output2 <- matrix(ncol=1, nrow=high-low+1)

a <- rnorm(popn, amean, sda)
b <- rnorm(popn, bmean, sdb)

for (j in low:high){
  for (i in 1:iterations){
    a1 <- sample(a, size=j, replace=FALSE)
    b1 <- sample(b, size=j, replace=FALSE)
    t <- t.test(a1,b1)
    output[i]<-round(t$p.value,3)
    sig <- length(which(output<0.05))
  }
  output2[j-low+1]<-(sig/iterations*100)
}

output2 <- data.frame(output2)
output2$n <- low:high
ggplot(output2, aes(x=n, y=output2))+geom_point()+
  ggtitle(paste('group means=',amean,",", bmean, sep=""), 
          paste("sd=",sda,",",sdb,"  population size=",popn, sep=""))+
  ylab("significant outputs (%)")+xlab("sample size")+theme_bw()
