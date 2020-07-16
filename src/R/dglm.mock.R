dglm.mock <- function(formula = num_awards ~ prog + math,family="poisson",tol= 1e-08,maxit=25) {

    datasets <- list(read.csv("./src/data/data_user1.csv"), read.csv("./src/data/data_user2.csv"),read.csv("./src/data/data_user3.csv"))

    client <- vtg::MockClient$new(datasets, "vtg.glm")
    results <- dglm(client, formula, family, tol)
}

#dglm.mock(formula = num_awards ~ prog + math,family=poisson,tol= 1e-08,maxit=25)
