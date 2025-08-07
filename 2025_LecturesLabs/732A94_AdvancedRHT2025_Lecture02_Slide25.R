.Random.seed<-rep(1L,626)
print(rexp(1))
.Random.seed<-rep(1L,626)
for (i in 1:1){print(rexp(1))}
.Random.seed<-rep(1L,626)
sapply(1:1,function(i){print(rexp(1))})
sapply(1:1,function(i){.Random.seed<-rep(1L,626);print(rexp(1))})
## https://stackoverflow.com/questions/30456481/controlling-seeds-with-mclapply
sapply(1:1,function(i){assign('.Random.seed', rep(1L,626), pos=.GlobalEnv);print(rexp(1))})