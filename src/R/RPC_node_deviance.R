RPC_node_deviance <- function(Data, weights = NULL, master) {
    #the function update the betas
    formula <- master$formula
    family <- master$family
    #the function calculate the residual deviance with updated betas for the single node
    y <- eval(formula[[2]], envir = Data) #extract y variable names
    X <- model.matrix(formula,data = Data) #extract X variables
    offset=model.offset(model.frame(formula, data = Data)) #extract the offset
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

    if(master$iter == 1){ #only for first iteration
        etastart = NULL
        nobs = nrow(X)    # needed by the initialize expression below
        nvars = ncol(X)   # needed by the initialize expression below
        eval(family$initialize) # initializes n and fitted values mustart
        eta = family$linkfun(mustart) + offset # we then initialize eta
        mu_old = family$linkinv(eta)
        dev_old = 0
    } else {
        mu_old <- family$linkinv(X %*% master$coef[,ncol(master$coef)-1])
        dev_old <- sum(family$dev.resids(y, mu_old,weights))
    }
    eta <- X %*% master$coef[,ncol(master$coef)] + offset #calcaute updated eta
    mu <- family$linkinv(eta - offset)
    dev <- sum(family$dev.resids(y, mu, weights)) #calculate new deviance
    dev.null <- sum(family$dev.resids(y, master$wtdmu, weights))
    output <- list(dev_old = dev_old, dev = dev, dev.null = dev.null)
    #saveRDS(output,file = paste0(userID,".Rds"))
    return(output)
}
