# dag - an object of class "dagitty" representing the variable DAG
# f.args - named list. Each element contains arguments for the g function. The element name denotes the child node name. 
# Each element is a list with optional arguments:
## link: "identity"/"quadratic/cosin". This is the link function. 
## levels: positive integer. 1 indicates continuous. 2 and above indicate the number of levels in a categorical variable
## betas: a list. Each element name indicate parent node. Each element contains beta coefficients (scalar for numeric parent, vector of length levles - 1 for factor with the firstt level being the baseline)
## labels: categorical variable labels. Need to be have length equal to the levels argument
## sinr: signal to noise ratio
# N number of samples to generate

gen_model_param <- function(dag, f.args = NULL){
  if(is.null(f.args)) f.args <- setNames(vector(mode = "list", length = length(names(dag))), nm = names(dag))
  if(sum(names(f.args) %in% names(dag)) < length(f.args)) stop("some variable entries in f.args don't match node names in supplied DAG")
  if(sum(duplicated(names(f.args)))>0) stop("duplicate f.args variable entries")
  
  vars <- names(dag)
  parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, var)), vars)
  
  for(var in vars){
    if(is.null(f.args[[var]]$link)) f.args[[var]]$link <- "identity"
    if(!f.args[[var]]$link %in% c("identity", "quadratic", "exp", "cosin")) stop(paste0("link argument in f.args for variable ,", var,  " has to be one of identity, quadratic, exp, cosin"))
    if(is.null(f.args[[var]]$levels)) f.args[[var]]$levels <- 1
    if(is.null(f.args[[var]]$labels) & f.args[[var]]$levels > 1) f.args[[var]]$labels <- LETTERS[1:f.args[[var]]$levels]
    if(is.null(f.args[[var]]$sinr)) f.args[[var]]$sinr <- 1
    for(parent in parents[[var]]){
      parent_levels <- f.args[[parent]]$levels
      if(is.null(parent_levels)) f.args[[parent]]$levels <- 1
      parent_betas <- f.args[[var]]$betas[[parent]]
      if(is.null(parent_betas)) f.args[[var]]$betas[parent] <- setNames(list(if(f.args[[parent]]$levels > 1) rnorm(f.args[[parent]]$levels - 1) else rnorm(1)), 
                                                                        nm = parent)
    }
  }
  return(f.args)
}
