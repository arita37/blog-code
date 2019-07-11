parametric_dag_model <-
function(dag, f.args = NULL){
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
  
  ans <- list(dag = dag, f.args = f.args)
  class(ans) <- "parametric_dag_model"
  return(ans)
}
