#' This contains the summary method for linreg function.
#' 
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

summary.linreg = function(object,...){
    cat("Call:\n ")
    print.default(as.vector(object$call))
    a=cbind(as.matrix(object$regression_coefficent),as.matrix(sqrt(diag(object$variance_reg_coef))),as.matrix(object$t_value), as.matrix(object$p_value), as.matrix(significance_value(object$p_value)) )
    colnames(a)=c("Estimate","std. Error","t value","Pr(>|t|)", " ")
    cat("\n Coefficents : \n")
    print.default(a,quote = FALSE)
    cat("\nResidual standard error:", sqrt(object$residual_variance), "on", object$degrees_of_freedom , "degrees of freedom")
}

significance_value=function(pval){
    signifcodes= ifelse(pval<0.001,"***",ifelse(pval<0.01,"**",ifelse(pval<0.05, "*",ifelse(pval<0.1,"."," "))))
}
