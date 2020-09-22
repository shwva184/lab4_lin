#' This contains the residuals of linreg function.
#' 
#' 
#' @param x An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

resid.linreg = function(x,...){
  if (!inherits(x, "linreg"))
    stop("This is not a \"linreg\" object.")
  if(length(x$residuals)){
    print.default(as.vector(x$residuals))
  } else {cat("regression not available \n")}
}


