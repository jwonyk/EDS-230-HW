#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, pars$pmort, pars$hunt, pars$threshold
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{alpha} is an interaction coefficient (higher values = greater interaction);
#'  \emph{eff} is the rate of ingestion of prey by predators;
#'  \emph{pmort} is the mortality rate of predator population;
#'  \emph{hunt} is the hunting rate applied to prey (proportion removed per day);
#'  \emph{threshold} is the minimum prey population required before hunting is permitted
#' @examples
#' lotvod(t = 1, pop = list(1, 2), pop = list(0.5, 0.3, 0.2, 0.2))
#'
#' pars <- c(rprey = 0.5, alpha = 0.3, eff = 0.2, pmort = 0.2)
#' currpop <- c(prey = 1, pred = 1)
#  days = seq(from=1,to=20)
#' res <- ode(func = lotvmod, y = currpop, times = days, parms = pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#' }

lotvmodK <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    
    if (prey >= threshold) {
      dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunt * prey
    } else {
      dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred
    }
    
    dpred <- eff * alpha * prey * pred - pmort * pred
    return(list(c(dprey, dpred)))
  })
}
