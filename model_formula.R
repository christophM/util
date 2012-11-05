###############################################################################
##
## Build model formulas
##
################################################################################


##' Function, which builds formulas
##'
##' .. content for \details{} ..
##' @title Build regression formula
##' @param df Data frame containing the variables
##' @param response Name of the response variable
##' @param fun Function, which the formula shall be used for 
##' @param tail String which shall be added at the end
##' @return The desired formula 
##' @author Christoph Molnar
build_form <- function(data, response, fun = "mboost", tail = character(0), df = 4){

  ## find all factors and numerics
  factor_indices <- sapply(data, is.factor)
  factor_vars <- setdiff(names(factor_indices[factor_indices]), response)
  numeric_vars <- setdiff(names(data), c(factor_indices, response))
  
  ## build the factor string part using bols
  factor_vars <- paste("bols(", factor_vars, ")", sep = "")

  ## build the numeric string part using bbs
  numeric_vars <- paste("bbs(", numeric_vars, ", df = ", df, ", center = TRUE)", sep = "")

  ## paste factors and numerics together
  effect_string <- paste(c(factor_vars, numeric_vars), collapse = " + ")

  ## add the response and finish formula
  form <- as.formula(paste(response, " ~ ",  effect_string, tail, sep = ""))
  return(form)
}
