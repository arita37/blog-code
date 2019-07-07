# dag - an object of class "dagitty" representing the variable DAG
# f.args - named list. Each element contains arguments for the g function. The element name denotes the child node name. 
# Each element is a list with optional arguments:
## link: "identity"/"quadratic/cosin". This is the link function. 
## levels: positive integer. 1 indicates continuous. 2 and above indicate the number of levels in a categorical variable
## betas: a list. Each element name indicate parent node. Each element contains beta coefficients (scalar for numeric parent, vector of length levles - 1 for factor with the firstt level being the baseline)
## labels: categorical variable labels. Need to be have length equal to the levels argument
## sinr: signal to noise ratio
# N number of samples to generate
# Make sure treatment values have the same values and level ordering as the original input!

get_ate <- function(dag, f.args = NULL,  treatment = NULL, treatment_vals = NULL, exposure = NULL, N = 1000, M = 1000){
  if(!identical(f.args, gen_model_param(dag = dag, f.args = f.args))) stop("f.args supplied does not specify all model parametrs. Consider using gen_model_param(f.args)")
  if(f.args[[exposure]]$levels > 2) stop("Exposure must be either numeric (levels = 1) or binary (levels = 2)")
  
  if(is.null(treatment_vals)){
    if(f.args[[treatment]]$levels == 1){
      sample_treatment <- sim_mixed_dag(dag = dag, f.args = f.args)[[treatment]]
      treatment_vals <- unname(quantile(sample_treatment, seq(0.05, 0.95, by = 0.3)))
    } else {
      treatment_vals <- f.args[[treatment]]$labels
    }
  }
  
  results <- matrix(ncol = length(treatment_vals), nrow = M)
  ans <- data.frame(treatment_value = treatment_vals, ate = NA)
  vars <- names(dag); vars <- vars[vars != treatment]
  parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, var)), vars)
  
  env <- environment()
  
  for(i in 1:length(treatment_vals)){
    input_treatment_list = setNames(list(rep(treatment_vals[i], N)), nm = treatment)
    for(m in 1:M){
      sim_data <- do.call(sim_mixed_dag, append(list(dag = dag, f.args = f.args, N = N), input_treatment_list))
      results[m, i] <- mean(sim_data[[exposure]])
    }
  }
  ans$ate <- c(NA, diff(apply(results, 2, mean)))

  return(ans)
}
