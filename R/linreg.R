#' linreg
#' @param formula A formula.
#' @param data A data.
#' @import stats
#' @description Implementing the calculations for multiple regression model using QR decomposition.
#' @return The function returns an object of class linreg as an S3 class
#' @references http://genomicsclass.github.io/book/pages/qr_and_regression.html
#' @export

linreg <-
  function(formula,data){
  call=match.call()
  X=model.matrix(formula,data)
  y=data[[all.vars(formula)[1]]]
  new_qr=qr(X)
  Q=qr.Q(new_qr)
  R=qr.R(new_qr)
  B=solve(R) %*% t(Q) %*% y                               #Regressions coefficients
  y1 = Q %*% t(Q) %*% y                                   #fitted value
  e  = y - y1                                             # residuals
  df = nrow(Q) - ncol(Q)                                  #degrees of freedom
  v= sum((y-y1)^2)/df                                     #residual variance
  v_rc = v * solve(t(R)%*% R)                             #variance of the regression coefficients    
  t_value = as.numeric(B) / as.numeric(sqrt(diag(v_rc)))  #t_value
  p_value <- 2 * pt(-abs(t_value), df)                    #p_value
  lst=list(call,B,y1,e,df,v,v_rc,t_value,p_value)
  names(lst)=c("call","regression_coefficent","fitted_values","residuals","degrees_of_freedom","residual_variance","variance_reg_coef","t_value","p_value")
  linreg=structure(lst, class = "linreg")
  return(linreg)
  }






