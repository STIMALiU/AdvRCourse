f_findsingleton<-function(v){
    if (!is.integer(v)){stop("Only integers can provided, e.g. c(1L,2L,2L)")}
    x<-0L
    for (i in v){x<-bitwXor(x,i)}
    x
}