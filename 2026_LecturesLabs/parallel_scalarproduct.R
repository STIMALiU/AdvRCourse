library(parallel)
N<-2^15 ## power of two for easiness of implementation
y<-rnorm(N)
x<-rnorm(N)


f_pardotprod<-function(x,y,numCores=4){
    cl <- makeCluster(getOption("cl.cores", numCores))

    vC<-parApply(cl,rbind(x,y),2,prod)
    p<-N/2

    while(p>0.5){
    ## a product can be done in the same way in parallel, replacing sum() with prod()
	vC<-parApply(cl,rbind(vC[1:p],vC[(p+1):(2*p)]),2,sum) 
	p<-p/2
    }
    vC
}

print(system.time(x%*%y))
print(system.time(f_pardotprod(x,y))) 
## the parallel implementation should be slower as R has %*% optimized
## and in parallel implementation we have overhead with creating data structures for parApply()

vC<-f_pardotprod(x,y)
print(c(vC,(x%*%y)[1,1]))
