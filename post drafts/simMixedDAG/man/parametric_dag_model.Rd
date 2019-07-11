\name{parametric_dag_model}
\alias{parametric_dag_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parametric_dag_model(dag, f.args = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dag}{
%%     ~~Describe \code{dag} here~~
}
  \item{f.args}{
%%     ~~Describe \code{f.args} here~~
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
function (dag, f.args = NULL) 
{
    if (is.null(f.args)) 
        f.args <- setNames(vector(mode = "list", length = length(names(dag))), 
            nm = names(dag))
    if (sum(names(f.args) \%in\% names(dag)) < length(f.args)) 
        stop("some variable entries in f.args don't match node names in supplied DAG")
    if (sum(duplicated(names(f.args))) > 0) 
        stop("duplicate f.args variable entries")
    vars <- names(dag)
    parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, 
        var)), vars)
    for (var in vars) {
        if (is.null(f.args[[var]]$link)) 
            f.args[[var]]$link <- "identity"
        if (!f.args[[var]]$link \%in\% c("identity", "quadratic", 
            "exp", "cosin")) 
            stop(paste0("link argument in f.args for variable ,", 
                var, " has to be one of identity, quadratic, exp, cosin"))
        if (is.null(f.args[[var]]$levels)) 
            f.args[[var]]$levels <- 1
        if (is.null(f.args[[var]]$labels) & f.args[[var]]$levels > 
            1) 
            f.args[[var]]$labels <- LETTERS[1:f.args[[var]]$levels]
        if (is.null(f.args[[var]]$sinr)) 
            f.args[[var]]$sinr <- 1
        for (parent in parents[[var]]) {
            parent_levels <- f.args[[parent]]$levels
            if (is.null(parent_levels)) 
                f.args[[parent]]$levels <- 1
            parent_betas <- f.args[[var]]$betas[[parent]]
            if (is.null(parent_betas)) 
                f.args[[var]]$betas[parent] <- setNames(list(if (f.args[[parent]]$levels > 
                  1) rnorm(f.args[[parent]]$levels - 1) else rnorm(1)), 
                  nm = parent)
        }
    }
    ans <- list(dag = dag, f.args = f.args)
    class(ans) <- "parametric_dag_model"
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
