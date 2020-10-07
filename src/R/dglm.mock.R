dglm.mock <- function(formula = num_awards ~ prog + math, types=NULL, dstar=NULL, family="poisson",tol= 1e-08,maxit=25) {

    datasets <- list(
        read.csv("./src/data/NL.csv"),
        read.csv("./src/data/IT.csv"),
        read.csv("./src/data/EU.csv")
    )

    client <- vtg::MockClient$new(datasets, "vtg.glm")
    results <- vtg.glm::dglm(client, formula=formula, family=family, types=types, tol=tol, dstar=dstar)
}
types=list(sex=list(type='factor',levels=1:2),
           site2=list(type='factor',levels=1:17),
           end=list(type='factor',levels=1:5),
           country1=list(type='factor',levels=1:20))
#dglm.mock(formula = d~end+sex+age+site2+country1+offset(log(y)),dstar = "d_star",types=types, family = 'rs.poi',maxit=25,tol= 1e-08)
