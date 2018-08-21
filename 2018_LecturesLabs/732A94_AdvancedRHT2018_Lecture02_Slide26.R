library(pryr)
`second<-`<-function(x, value){
    x[2]<-value
    x
}
x<-1:10
print(x)
print(address(x))
second(x)<-5L
print(x)
print(address(x))

print("==========================")
y<-1:10
print(y)
print(address(y))
y[2]<-5L
print(y)
print(address(y))
print("===========================")
## additional arguments
`modify<-`<-function(x, position, value){
    x[position]<-value
    x
}
modify(x,1)<-10
print(x)


test_addr<-function(){
    print("Now doing the same but inside a called function")
    y<-1:10
    print(y)
    print(address(y))
    y[2]<-5L
    print(y)
    print(address(y))
    print("===========================")
    NA
}

test_addr()
