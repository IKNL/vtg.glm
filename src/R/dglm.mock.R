dglm.mock <- function(formula = num_awards ~ prog + math, types=NULL, dstar=NULL, family="poisson",tol= 1e-08,maxit=25) {

    datasets <- list(
        read.csv("C:/Users/mce1908.52713/Desktop/Eurocare/Data_NL/NL.csv"),
        read.csv("C:/Users/mce1908.52713/Desktop/Eurocare/Data_NL/IT.csv"),
        read.csv("C:/Users/mce1908.52713/Desktop/Eurocare/Data_NL/EU.csv")
    )

    client <- vtg::MockClient$new(datasets, "vtg.glm")
    results <- vtg.glm::dglm(client, formula, family, tol)
}
types=list(sex=list(type='factor',levels=1:2),
           site2=list(type='factor',levels=1:17),
           end=list(type='factor',levels=1:5),
           country1=list(type='factor',levels=1:20))
#dglm.mock(formula = d~end+sex+age+site2+country1+offset(log(y)),dstar = "d_star",types=types, family = 'rs.poi',maxit=25,tol= 1e-08)
