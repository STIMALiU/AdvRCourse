Rprof(tmp <- tempfile(), line.profiling = TRUE, memory.profiling = TRUE)
test_data <- pxweb::get_pxweb_data(
  url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
  dims = list(Region = c('*'), 
              Civilstand = c('*'), 
              Alder = c('*'), 
              Kon = c('*'), 
              ContentsCode = c('*'),
              Tid = as.character(1970)),
  clean = TRUE)
Rprof()
summaryRprof(tmp, lines = "show", memory = "both")