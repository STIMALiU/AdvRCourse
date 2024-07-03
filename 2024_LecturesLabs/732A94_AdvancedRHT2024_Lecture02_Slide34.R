## This code will behave differently on different versions of R
## Form experiments it seems that with newer versions it is impossible
## to have modification in place (cf R-3.3.1 with R-3.4.4 and later)

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
## on R-3.3.1 or earlier (?) try manually typing the below code
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
