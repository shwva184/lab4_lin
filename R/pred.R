#'Generic method for getting the fitted values
#'
#' @rdname pred
#' @param p An object
#' @param ... Further arguments passed to or from other methods
#' 
#' 
#' @export

pred = function(p, ...){
  UseMethod("pred")
}

#' This contains the fitted values of linreg function.
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' 
#' @rdname pred
#' @method pred linreg
#' 
#' @return This prints out predicted or fitted values for the object passed.
#' @export

pred.linreg= function(p,...){
  if (!inherits(p, "linreg")){
    stop("This is not a \"linreg\" object.")}
  return(as.vector(p$fitted_values))
}



