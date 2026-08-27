library(profvis)
library(pxweb)

## In this example not much interesting will be seen
Rprof(tmp <- tempfile(),  line.profiling = TRUE,   memory.profiling = TRUE)
##test_data <- pxweb::get_pxweb_data(
url_toquery <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
## the below creates a json file inside your R library 
query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
test_data <- pxweb::pxweb_get_data(
   url = url_toquery, query = query )
Rprof()
print(summaryRprof(tmp,lines="show"))
print(profvis::parse_rprof(path =tmp))

Rprof(tmp <- tempfile())
example(glm)
Rprof()
print(summaryRprof(tmp))




p<-profvis::profvis({
    url_toquery <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
    query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
    test_data <- pxweb::pxweb_get_data(url = url_toquery, query = query )
})
print(p) ## will display a HTML page

