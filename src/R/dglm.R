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


    # Run in a MASTER container
    if (client$use.master.container) {
        vtg::log$debug(glue::glue("Running `dglm` in master container using image '{image.name}'"))
        result <- client$call("dglm", formula, family, tol, maxit)
        return(result)
    }

    # results <- client$call("node_beta", master=master)
    # print(results)
    master <- list(formula = formula, family = family, iter = 1, tol = tol,
                   maxit = maxit)


    repeat{

        vtg::log$info(glue::glue("--> I am on iteration {master$iter}."))
        results <- client$call("node_beta", master = master)
        # print(length(results))

        vtg::log$debug(glue::glue("--> length of results = {length(results)}"))
        Ds <- lapply(results, as.data.frame)

        vtg::log$debug("Master beta")
        master <- vtg.glm::master_beta(master= master, nodes = results)

        vtg::log$debug(glue::glue("--> length of results = {length(results)}"))
        results <- client$call("node_deviance", master = master)
        Ds <- lapply(results, as.data.frame)

        vtg::log$debug("Master deviance")
        master <- vtg.glm::master_deviance(nodes = results, master = master)
        if (master$converged) {
            vtg::log$debug("Converged.")
            break
        }
    }
    return(master)
}