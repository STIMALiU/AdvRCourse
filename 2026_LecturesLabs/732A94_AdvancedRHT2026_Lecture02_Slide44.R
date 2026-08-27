counter_factory<- function (){i<-0;f<-function(){i<<-i+1;i};f}
f.c<-counter_factory()
s.c<-counter_factory()
f.c() ##1
f.c() ##2
s.c() ##1
ls(environment(f.c)) ## "f" "i"
environment(f.c)$i ## 2
environment(s.c)$i ## 1
