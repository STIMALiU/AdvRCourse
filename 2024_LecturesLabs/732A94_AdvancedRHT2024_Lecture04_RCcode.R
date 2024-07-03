# Create object with fields and methods
Account <- setRefClass("Account",
    fields = list(balance = "numeric"),
    methods = list(
	withdraw = function(x) {
	    balance <<- balance - x
	},
	deposit = function(x) {
	    balance <<- balance + x
	}
    )
)
a<-Account$new(balance=100)

a$balance<-200; a$balance ##output: 200
b<-a;b$balance ##output: 200
a$balance<-0;b$balance ##output: 0

c<-a$copy() ## all RC objects have a copy() method

a$balance<-100;c$balance;a$balance ##output: 0, 100

## constructor
Account$methods(initialize=function(balance=0){.self$balance<<-balance;cat("New account created!\n")})
d<-Account(300)
e<-Account()

## default printing
print.Account<-function(x){cat(paste("Account balance: ",x$balance,"\n"))} ## but this will not do print nicely if object is just typed
Account$methods(show=function(){cat(paste0("Account balance: ",.self$balance,"\n"))}) ## this works for both calling print or just typing



