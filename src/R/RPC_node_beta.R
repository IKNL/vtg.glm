RPC_node_beta <- function(Data, weights = NULL, master = NULL) {
    #if(is.null(user)){ print("Please specify the user number (1,2,3,....)"); break}
    vtg::log$debug("Starting the node beta.")
    formula <- master$formula
    family <- master$family
    y <- eval(formula[[2]], envir = Data) #extract y and X varibales name from formula
    X <- model.matrix(formula, data = Data) #create a model matrix
    offset <- model.offset(model.frame(formula, data = Data)) #extract the offset from formula (if exists)
    #functions of the family required (gaussian, poisson, logistic,...)
    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }

    if (is.null(weights)) weights <- rep.int(1, nrow(X))
    if (is.null(offset)) offset <- rep.int(0, nrow(X))

    nobs <- nrow(X)    # needed by the initialize expression below
    nvars <- ncol(X)   # needed by the initialize expression below

    if (master$iter==1) {
        etastart = NULL
        eval(family$initialize) # initializes n and fitted values mustart
        eta = family$linkfun(mustart) # we then initialize eta with this
    } else {
        eta = (X %*% master$coef[,ncol(master$coef)]) + offset #update eta
    }

    mu <-  family$linkinv(eta)
    varg <- family$variance(mu)
    gprime <- family$mu.eta(eta)
    z <- (eta - offset) + (y - mu) / gprime #calculate z
    W <- weights * as.vector(gprime^2 / varg) #update the weights
    dispersion <- sum(W *((y - mu) / family$mu.eta(eta))^2) #calculate the dispersion matrix
    output <- list(v1 = crossprod(X, W*X), v2 = crossprod(X, W*z), dispersion = dispersion,
                nobs = nobs, nvars = nvars, wt1 = sum(weights * y), wt2 = sum(weights))
    #saveRDS(output,file = paste0(userID,'.Rds'))
    return(output)
}
