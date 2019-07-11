get_ate.parametric_dag_model <-
function(dag_model,  treatment = NULL, treatment_vals = NULL, exposure = NULL, N = 1000, M = 1000){
  dag <- dag_model$dag; f.args <- dag_model$f.args
  if(f.args[[exposure]]$levels > 2) stop("Exposure must be either numeric (levels = 1) or binary (levels = 2)")
  
  if(is.null(treatment_vals)){
    if(f.args[[treatment]]$levels == 1){
      sample_treatment <- sim_mixed_dag(dag_model)[[treatment]]
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
    treatment_ls = setNames(list(rep(treatment_vals[i], N)), nm = treatment)
    for(m in 1:M){
      sim_data <- do.call(sim_mixed_dag, list(dag_model = dag_model, N = N, treatment_list = treatment_ls))
      results[m, i] <- mean(sim_data[[exposure]])
    }
  }
  ans$ate <- c(NA, diff(apply(results, 2, mean)))
  
  return(ans)
}
