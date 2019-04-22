dmsTOdd <- function(data, type = 'lat') {
  # Data is in DDMMSS.SS, needs to be transformed in degree decimals (Dd)
  # The formula is: Dd = DD + MM/60 + SS.SS/3600
  dd <- as.numeric(substr(data,2,3)) # degrees
  mm <- as.numeric(substr(data,4,5)) / 60 # minutes
  ss <- as.numeric(substr(data,6,(nchar(data)-1))) / 3600 # minutes
  Dd <- dd + mm + ss
  if(type == 'long') Dd <- -Dd
  return(Dd)
}
