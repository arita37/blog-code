\name{sim_mixed_dag.parametric_dag_model}
\alias{sim_mixed_dag.parametric_dag_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sim_mixed_dag.parametric_dag_model(dag_model, N = 1000, treatment_list = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dag_model}{
%%     ~~Describe \code{dag_model} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{treatment_list}{
%%     ~~Describe \code{treatment_list} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dag_model, N = 1000, treatment_list = list()) 
{
    dag <- dag_model$dag
    f.args <- dag_model$f.args
    if (sum(names(f.args) \%in\% names(dag)) < length(f.args)) 
        stop("some variable entries in f.args don't match node names in supplied DAG")
    env = environment()
    list2env(treatment_list, envir = env)
    vars <- names(dag)
    parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, 
        var)), vars)
    if (sum(duplicated(names(f.args))) > 0) 
        stop("duplicate f.args variable entries")
    f <- function(vars, link = "identity", levels = 1L, betas = NULL, 
        labels = NULL, sinr = 1, env = NULL, result_var = NULL) {
        if (!link \%in\% c("identity", "quadratic", "exp", "cosin")) 
            stop(paste0("link argument in f.args for variable \"", 
                result_var, "\" has to be one of identity, quadratic, exp, cosin"))
        lp <- rep(0, N)
        for (var in vars) {
            var_val <- try(eval(parse(text = var), envir = parent.frame(1L)), 
                silent = T)
            if (class(var_val) == "try-error") {
                assign(var, do.call(f, append(list(vars = parents[[var]], 
                  result_var = var), f.args[[var]])), envir = pos.to.env(-1L))
                assign(var, eval(parse(text = var)), envir = env)
                var_val <- eval(parse(text = var))
            }
            if (is.null(betas[[var]])) {
                beta <- as.matrix(if (is.factor(var_val)) 
                  rnorm(length(levels(var_val)) - 1)
                else rnorm(1))
            }
            else {
                beta <- as.matrix(betas[[var]])
            }
            X <- model.matrix(~var_val - 1)
            if (is.factor(var_val)) 
                X <- X[, -1]
            if (ncol(X) != nrow(beta)) 
                stop(paste0("Number of levels in \"", var, "\" does not match number of input betas for that variable"))
            lp <- lp + as.numeric(X \%*\% beta)
        }
        if (link == "quadratic") 
            lp <- sign(lp) * lp^2
        if (link == "cosin") 
            lp <- cos(lp * 2)
        if (link == "exp") 
            lp <- exp(lp) - 1
        if (var(lp) == 0) {
            lp <- lp + rnorm(length(lp), sd = 1)
        }
        else {
            lp <- lp + rnorm(length(lp), sd = sqrt(var(lp)/sinr))
        }
        if (levels > 1) {
            if (is.null(labels)) 
                labels <- LETTERS[1:levels]
            if (length(labels) != levels) 
                stop(paste0("labels do not match levels argument for variable \"", 
                  result_var, "\""))
            lp <- lp/sd(lp)
            thresh <- qnorm(p = seq(0, 1, length.out = levels + 
                1))
            lp <- cut(lp, breaks = thresh, labels = labels, include.lowest = T)
        }
        return(lp)
    }
    for (var in vars) {
        if (!exists(var, envir = env)) {
            assign(var, do.call(f, append(list(vars = parents[[var]], 
                env = env, result_var = var), f.args[[var]])), 
                envir = env)
        }
    }
    return(setNames(data.frame(lapply(vars, function(var) eval(parse(text = var)))), 
        vars))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
