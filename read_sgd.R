require(XML)
library(data.table)
library(clickR)

read_sgd <- function(file, k=8, ...){
  data <- xmlParse(file, ...)
  xml_data <- xmlToList(data)
  if(any(sapply(xml_data$COLUMNS, function(x) x["Type"]) == "N")){
    datos <- fix_factors(fix_numerics(as.data.frame(rbindlist(lapply(xml_data[names(xml_data) %in% "ROW"], function(x){
      as.data.frame(do.call(cbind, x))
    }), fill=TRUE))), k=k)
  } else{
    datos <- fix_factors(as.data.frame(rbindlist(lapply(xml_data[names(xml_data) %in% "ROW"], function(x){
      as.data.frame(do.call(cbind, x))
    }), fill=TRUE)), k=k)
  }
  names(datos) <- sapply(xml_data$COLUMNS, function(x) x["Name"])
  datos
}
