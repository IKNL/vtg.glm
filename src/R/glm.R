# master_init=function(formula,family=gaussian,tol= 1e-08,maxit=25) {
#     saveRDS(list(formula=formula,family=family,iter=1,tol= tol,maxit=maxit),"master.Rds")
# }


#' Run the distributed GLM algorithm.
#'
#' Params:
#'    client: ptmclient::Client instance.
#'    formula: dependant_variable ~ explanatory_variable(i) + ...
#'    family: to this up it uses the Gaussian as this is the default value
#'    tol: tolerance level
#'    maxit: maximum number of iterations the function is allowed to cycle up to
#'
dglm <- function(client, formula, family = gaussian, tol = 1e-08, maxit = 25) {

    USE_VERBOSE_OUTPUT <- getOption('vtg.verbose_output', T)
    lgr::threshold("debug")
    image.name <- "harbor.vantage6.ai/vantage/vtg.glm"

    client$set.task.image(
        image.name,
        task.name <- "GLM"
    )

    vtg::log$debug("Initialising.")

    master <- list(formula = formula, family = family, iter = 1, tol = tol,
                   maxit = maxit)

    # results <- client$call("node_beta", master=master)
    # print(results)

    repeat{
        # print(master$iter)
        vtg::log$info(glue::glue("I am on iteration {master$iter}."))
        results <- client$call("node_beta", master = master)
        # print(length(results))
        vtg::log$debug(glue::glue("length of results = {length(results)}"))
        Ds <- lapply(results, as.data.frame)
        master <- master_beta(master= master, nodes = results)
        results <- client$call("node_deviance", master = master)
        Ds <- lapply(results, as.data.frame)
        master <- master_deviance(nodes = results, master = master)
        if (master$converged) {
            break
        }
    }
    return(master)
}