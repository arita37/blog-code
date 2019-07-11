\name{get_ate.parametric_dag_model}
\alias{get_ate.parametric_dag_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_ate.parametric_dag_model(dag_model, treatment = NULL, treatment_vals = NULL, exposure = NULL, N = 1000, M = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dag_model}{
%%     ~~Describe \code{dag_model} here~~
}
  \item{treatment}{
%%     ~~Describe \code{treatment} here~~
}
  \item{treatment_vals}{
%%     ~~Describe \code{treatment_vals} here~~
}
  \item{exposure}{
%%     ~~Describe \code{exposure} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
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
function (dag_model, treatment = NULL, treatment_vals = NULL, 
    exposure = NULL, N = 1000, M = 1000) 
{
    dag <- dag_model$dag
    f.args <- dag_model$f.args
    if (f.args[[exposure]]$levels > 2) 
        stop("Exposure must be either numeric (levels = 1) or binary (levels = 2)")
    if (is.null(treatment_vals)) {
        if (f.args[[treatment]]$levels == 1) {
            sample_treatment <- sim_mixed_dag(dag_model)[[treatment]]
            treatment_vals <- unname(quantile(sample_treatment, 
                seq(0.05, 0.95, by = 0.3)))
        }
        else {
            treatment_vals <- f.args[[treatment]]$labels
        }
    }
    results <- matrix(ncol = length(treatment_vals), nrow = M)
    ans <- data.frame(treatment_value = treatment_vals, ate = NA)
    vars <- names(dag)
    vars <- vars[vars != treatment]
    parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, 
        var)), vars)
    env <- environment()
    for (i in 1:length(treatment_vals)) {
        treatment_ls = setNames(list(rep(treatment_vals[i], N)), 
            nm = treatment)
        for (m in 1:M) {
            sim_data <- do.call(sim_mixed_dag, list(dag_model = dag_model, 
                N = N, treatment_list = treatment_ls))
            results[m, i] <- mean(sim_data[[exposure]])
        }
    }
    ans$ate <- c(NA, diff(apply(results, 2, mean)))
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
