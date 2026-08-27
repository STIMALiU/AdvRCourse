## read also on https://rdrr.io/r/methods/NextMethod.html for ambiguities

# Create class with slots (with permitted classes)
setClass("Person",
slots=list(name="character", age="numeric", 
 salary="numeric"))

setClass("Computer",
slots=list(hostname="character", num_cores="numeric"))


# Create inheriting class, can inherit from multiple
setClass("Employee",
slots=list(boss="Person"),contains=c("Person","Computer"))


## new is the constructor

setGeneric("salary_change", function(p, i) {
   standardGeneric("salary_change")
 })
setMethod("salary_change",
signature(p = "Person", i = "numeric"), 
  function(p, i) {
  print("salary_change.Person() called")
    p@salary+i
  })
setMethod("salary_change",
signature(p = "Computer", i = "numeric"), 
  function(p, i) {
  print("salary_change.Computer() called")
    p@num_cores+i
  })


setMethod("salary_change",
signature(p = "Employee", i = "numeric"), 
  function(p, i) {
     nsal<-callNextMethod() 
     ## method from parent (contained) class
     if (nsal>p@boss@salary){nsal<-p@salary}
     nsal
  })


alice<-new("Person", name="Alice",age=40, salary=100)
bob<-new("Employee", name="Bob",age=25, salary=90, boss=alice,hostname="bobsmachine",num_cores=4)
print(salary_change(bob,5))

setClass("Employee2",
slots=list(boss="Person"),contains=c("Computer","Person"))


setMethod("salary_change",
signature(p = "Employee2", i = "numeric"), 
  function(p, i) {
     nsal<-callNextMethod() 
     ## method from parent (contained) class
     if (nsal>p@boss@salary){nsal<-p@salary}
     nsal
  })

bob2<-new("Employee2", name="Bob",age=25, salary=90, boss=alice,hostname="bobsmachine",num_cores=4)

print(salary_change(bob2,5))
