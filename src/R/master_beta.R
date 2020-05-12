master_beta <- function(..., nodes = NULL, master = NULL) {
    #receive as many object as many are the nodes involved in the analysis (...)
    #the function update the betas
    vtg::log$debug("Starting master Beta.")
    formula <- master$formula
    family <- master$family
    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }
    if (is.null(nodes)) {
        g <- list(...)  #place the dots into a list
    } else {
        g <- nodes
    }
    vtg::log$debug("Merging node calculation to update new Betas.")
    allwt <- Reduce(`+`, lapply(1:length(g), function(j) g[[j]]$wt2)) #total sum of weights
    wtdmu <- Reduce(`+`, lapply(1:length(g), function(j) g[[j]]$wt1/allwt)) #global weighted mu
    a <- Reduce(`+`, lapply(1:length(g), function(j) g[[j]]$v1)) #sum up components of the matrix to be inverted calculated in each node
    b <- Reduce(`+`, lapply(1:length(g), function(j) g[[j]]$v2)) #sum up components of the matrix to be inverted calculated in each node
    phi <- Reduce(`+`, lapply(1:length(g), function(j) g[[j]]$dispersion)) # sum up components dispersion matrix
    nobs <- Reduce(`+`, lapply(1:length(g), function(j) g[[j]]$nobs)) #total number of observation
    nvars <- nrow(g[[1]]$v1) #number of variables

    if (is.null(master)) {
        beta <- rep(1, nvars)
    } else {
        beta <- master$coef
    }
    if (family$family %in% c("poisson", "binomial")) {
        disp <- 1
        est.disp <- FALSE
    } else {
        disp <- phi / (nobs - nvars)
        est.disp <- T
    }
    vtg::log$debug("Updating the Betas.")
    fb <- solve(a, b, tol = 2 * .Machine$double.eps) #calculate the new betas
    se <- sqrt(diag(solve(a) * disp)) #calculate the Standard error of coefficients

    master$coef <- cbind(master$coef, fb)
    master$se <- se
    master$disp <- disp
    master$est.disp <- est.disp
    master$nobs <- nobs
    master$nvars <- nvars
    master$wtdmu <- wtdmu

    #saveRDS(master,file = paste0("master.Rds"))
    #return(output)
    return(master)
}