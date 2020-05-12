as.GLM <- function(obj, data=NULL) {
  #fill a GLM object with output of the Federated Learning GLM
  dots <- as.list(obj$coefficients)

  out<-list()

  tt <- terms(obj$formula, data=data)

  if(!is.null(data)) {

    mf <- model.frame(tt, data)

    vn <- sapply(attr(tt, "variables")[-1], deparse)


    tt=M_2$terms
    vn <- sapply(attr(tt, "variables")[-1], deparse)



    if((yvar <- attr(tt, "response"))>0)

      vn <- vn[-yvar]

    xlvl <- lapply(data[vn], function(x) if (is.factor(x))

      levels(x)

      else if (is.character(x))

        levels(as.factor(x))

      else

        NULL)

    attr(out, "xlevels") <- xlvl[!vapply(xlvl,is.null,NA)]

    attr(tt, "dataClasses") <- sapply(data[vn], stats:::.MFclass)

  }

  out$terms <- tt

  coef <- numeric(0)

  stopifnot(length(dots)>1 & !is.null(names(dots)))

  for(i in seq_along(dots)) {

    if((n<-names(dots)[i]) != "") {

      v <- dots[[i]]

      if(!is.null(names(v))) {

        coef[paste0(n, names(v))] <- v

      } else {

        stopifnot(length(v)==1)

        coef[n] <- v

      }

    } else {

      coef["(Intercept)"] <- dots[[i]]

    }

  }

  out$coefficients <- coef

  out$rank <- length(coef)

  family=obj$family

  if (!missing(family)) {

    out$family <- if (class(family) == "family") {

      family

    } else if (class(family) == "function") {

      family()

    } else if (class(family) == "character") {

      get(family)()

    } else {

      stop(paste("invalid family class:", class(family)))

    }

    out$qr <- list(pivot=seq_len(out$rank))

    out$deviance <- obj$deviance

    out$null.deviance <- obj$null.deviance

    out$aic <- 1

    out$iter=obj$iter

    out$df.null=obj$nobs-1

    out$df.residual=obj$nobs-out$rank

    out$call=call("glm_FL",f,family=obj$family$family)

    class(out) <- c("glm","lm")


  } else {

    class(out) <- "lm"

    out$fitted.values <- predict(out, newdata=dd)

    out$residuals <- out$mf[attr(tt, "response")] - out$fitted.values

    out$df.residual <- nrow(data) - out$rank

    out$model <- data

    #QR doesn't work

  }

  out

}

summary_FL_GLM <- function(obj, ...){
  #Summarize the output of the GLM in a user-friendly tableS
  sign=cut(obj$pvalue,c(0,0.001,0.01,0.05,0.1,1),include.lowest = T,labels=c('***','**','*','.',''))
    cat("Call:\n")
    print(call("glm_FL",obj$formula,family=obj$family$family))
 # print(obj$formula)

  DF=data.frame(round(obj$coefficients,5),round(obj$Std.Error,5),round(obj$zvalue,3),round(obj$pvalue,5),sign)
  if(obj$est.disp){
    names(DF)=c("Estimate","Std. Error","t value","Pr(>|t|)","")
  }else{
    names(DF)=c("Estimate","Std. Error","z value","Pr(>|z|)","")
  }
  cat("\nCoefficients:\n")
  print(DF)

  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 \n\n")
  cat(paste0('(Dispersion parameter for ',obj$family$family," family taken to be ",round(obj$dispersion,5),')\n \n'))
  cat(paste0("Null deviance: ",round(obj$null.deviance,2),"  on ",obj$nobs-1," degrees of freedom \n"))
  cat(paste0("Residual deviance: ",round(obj$deviance,2),"  on ",obj$nobs-obj$nvars," degrees of freedom \n \n"))
  cat(paste0('Number of Fisher Scoring iterations: ',obj$iter,'\n'))

}

FL_GLM=function(...,f,family,maxit=25,tol=1e-08){
  data=list(...)
  Master_1=NULL
  for(j in 1:maxit){
    Node_1=lapply(data,function(p) node_beta(formula = f,Data = p,beta = Master_1,iter = j,family =family))
    Master_1=master_beta(nodes=Node_1,beta = Master_1,family = family)
    Node_2=lapply(data,function(p) node_deviance(formula = f,Data = p,beta = Master_1,iter = j,family =family))
    Master_2=master_deviance(nodes=Node_2,beta=Master_1,iter=j,family = family,formula = f,maxit=maxit)
    if(Master_2$converged)break
  }
  return(Master_2)
}



