#' This is creating an s3 method pred for linreg class.
#'
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

pred = function(object, ...){
  UseMethod("pred")
}

#' This contains the fitted values of linreg function.
#'
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @return This prints out predicted or fitted values for the object passed.
#' @export

pred.linreg= function(object,...){
  if (!inherits(object, "linreg")){
    stop("This is not a \"linreg\" object.")}
  if(length(object$fitted_values)){
    print.default(as.vector(object$fitted_values))
  } else {cat("Fitted values not available \n")}
}



