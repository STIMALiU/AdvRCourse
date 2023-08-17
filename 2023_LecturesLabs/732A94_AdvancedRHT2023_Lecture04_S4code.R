# Create class with slots (with permitted classes)
setClass("Person",
slots=list(name="character", age="numeric", 
 salary="numeric"))
# Create inheriting class, can inherit from multiple
setClass("Employee",
slots=list(boss="Person"),contains="Person")


## new is the constructor
alice<-new("Person", name="Alice",age=40, salary=100)
print(alice@age)
bob<-new("Employee", name="Bob",age=25, salary=100, boss=alice)

setGeneric("salary_change", function(p, i) {
   standardGeneric("salary_change")
 })
setMethod("salary_change",
signature(p = "Person", i = "numeric"), 
  function(p, i) {
    p@salary+i
  })
setMethod("salary_change",
signature(p = "Employee", i = "numeric"), 
  function(p, i) {
     nsal<-callNextMethod() 
     ## method from parent (contained) class
     if (nsal>p@boss@salary){nsal<-p@salary}
     nsal
  })

## ===================================================================
## how to print
print.Person<-function(x){
    cat(paste0(x@name,": ",x@age,"y/o, salary: ",x@salary))
    cat("\n")    
}
print.Employee<-function(x){
    cat(paste0(x@name,": ",x@age,"y/o, salary: ",x@salary))
    if (is.element("boss",slotNames(x))){
	cat(paste0(" and works under ",x@boss@name))
    }
    cat("\n")    
}

print(alice)
print(bob)  ## works nicely, but if you just type bob then you will get something very different unlike in S3

## One needs to define a function called "show" associated with the class that allows for a default display of the object
## defining show.Person() will NOT work
setMethod("show","Person", 
    function(object){ ## warning raised if called different than object
    cat(paste0(object@name,": ",object@age,"y/o, salary: ",object@salary))
    cat("\n")    
})

