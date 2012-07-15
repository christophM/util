
##' Turn a factor in multiple dichotom factors
##'
##' This function takes a vector with an encoded factor and splits it up in a data.frame
##' with one column for each factor level. The value is one if the factor has the certain level, else
##' it is 0. 
##' @title Dichotomize a factor
##' @param fac A factor vector
##' @return A data.frame with columns for each factor level
##' @author chris
dichotomize <- function (fac, name){
  fac_unique <- unique(fac)
  df <- as.data.frame(sapply(fac_unique,
                             function(x) {
                               ifelse(fac == x, 1, 0)
                             }))
  colnames(df) <- paste(name, fac_unique, sep = "_")
  df
}


