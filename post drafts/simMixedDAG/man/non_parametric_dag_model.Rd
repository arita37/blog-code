\name{non_parametric_dag_model}
\alias{non_parametric_dag_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
non_parametric_dag_model(dag, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dag}{
%%     ~~Describe \code{dag} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (dag, data) 
{
    if (mean(names(dag) \%in\% names(data)) != 1) 
        stop("Some DAG nodes not found in data")
    if (any(is.na(data))) 
        stop("data contains missing values")
    data <- data[names(dag)]
    data <- data \%>\% mutate_if(is.character, factor) \%>\% mutate_if(function(x) length(unique(x)) == 
        2, factor)
    vars <- names(dag)
    ans <- setNames(object = vector(mode = "list", length = length(vars)), 
        nm = vars)
    for (var in vars) {
        var_parents <- parents(dag, var)
        if (length(var_parents) == 0) {
            ans[[var]]$node_type <- "parentless"
            sampling_vals <- tapply(data[[var]], data[[var]], 
                length)/length(data[[var]])
            sampling_vals <- data.frame(value = names(sampling_vals), 
                freq = sampling_vals, row.names = NULL, stringsAsFactors = F)
            if (is.numeric(data[[var]])) 
                sampling_vals$value = as.numeric(sampling_vals$value)
            ans[[var]]$sampling_vals <- sampling_vals
            ans[[var]]$target_levels <- if (is.factor(data[[var]])) 
                levels(data[[var]])
            else NULL
        }
        else {
            ans[[var]]$parents <- var_parents
            if (class(data[[var]]) == "factor") {
                ans[[var]]$node_type <- "categorical"
                forms <- lapply(unlist(strsplit(paste0(var, paste0(rep(paste0(" ~ ", 
                  paste0(sapply(var_parents, function(var_parent) {
                    if (class(data[[var_parent]]) == "factor") return(var_parent) else return(paste0("s(", 
                      var_parent, ")"))
                  }), collapse = " + ")), length(levels(data[[var]])) - 
                  1), collapse = ", ")), ",")), as.formula)
                ans[[var]]$target_levels <- levels(data[[var]])
                num_levels <- length(levels(data[[var]]))
                dat <- data[c(var, var_parents)]
                dat[[var]] <- as.integer(dat[[var]]) - 1
                ans[[var]]$gam_model <- gam(forms, family = if (num_levels == 
                  2) 
                  "binomial"
                else multinom(K = num_levels - 1), data = dat)
            }
            else {
                ans[[var]]$node_type <- "continuous"
                form <- as.formula(paste0(var, " ~ ", paste0(sapply(var_parents, 
                  function(var_parent) {
                    if (class(data[[var_parent]]) == "factor") 
                      return(var_parent)
                    else return(paste0("s(", var_parent, ")"))
                  }), collapse = " + ")))
                dat <- data[c(var, var_parents)]
                ans[[var]]$gam_model <- gam(formula = form, family = "gaussian", 
                  data = dat)
                ans[[var]]$gam_model$sd <- sd(dat[[var]])
            }
        }
    }
    ans <- list(dag = dag, gam_fits = ans)
    class(ans) <- "non_parametric_dag_model"
    return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
