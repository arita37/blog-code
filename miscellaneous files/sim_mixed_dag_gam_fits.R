# dag - an object of class "dagitty" representing the variable DAG
# f.args - named list. Each element contains arguments for the g function. The element name denotes the child node name. 
# Each element is a list with optional arguments:
## link: "identity"/"quadratic/cosin". This is the link function. 
## levels: positive integer. 1 indicates continuous. 2 and above indicate the number of levels in a categorical variable
## betas: a list. Each element name indicate parent node. Each element contains beta coefficients (scalar for numeric parent, vector of length levles - 1 for factor with the firstt level being the baseline)
## labels: categorical variable labels. Need to be have length equal to the levels argument
## sinr: signal to noise ratio
# N number of samples to generate
## ... set variables using this

sim_mixed_dag_gam_fits <- function(dag, gam_fits,  N){
  if(mean(names(gam_fits) %in% names(dag)) != 1 | length(names(dag)) != length(names(gam_fits))) stop("some variable entries in gam_fits don't match node names in supplied DAG")
  
  env <- environment()
  vars <- names(dag)
  
  for(var in vars){
    if(!exists(var, envir = env)){ 
      if(gam_fits[[var]]$node_type == "parentless"){
        assign(var, do.call(f_parentless, list(sampling_vals = gam_fits[[var]]$sampling_vals, n = N, 
                                               target_levels = gam_fits[[var]]$target_levels)), pos = env) 
      } else if(gam_fits[[var]]$node_type == "categorical") {
        assign(var, do.call(f_categorical, list(gam_model = gam_fits[[var]]$gam_model, 
                                                parents = gam_fits[[var]]$parents, 
                                                target_levels = gam_fits[[var]]$target_levels, 
                                                n = N, 
                                                env = env)), pos = env) 
      } else if(gam_fits[[var]]$node_type == "continuous"){
        assign(var, do.call(f_cont, list(gam_model = gam_fits[[var]]$gam_model, 
                                                parents = gam_fits[[var]]$parents, 
                                                n = N, 
                                                env = env)), pos = env) 
      }
      
    }
  }
  return(setNames(data.frame(lapply(vars, function(var) eval(parse(text = var)))), vars))
}
