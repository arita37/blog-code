get_ate.non_parametric_dag_model <-
function(dag_model,  treatment = NULL, treatment_vals = NULL, exposure = NULL, N = 1000, M = 1000){
  dag <- dag_model$dag; gam_fits <- dag_model$gam_fits
  if(gam_fits[[exposure]]$node_type != "continuous") stop("Exposure must be continuous")
  
  if(is.null(treatment_vals)){
    sample_treatment <- sim_mixed_dag(dag_model)[[treatment]]
    treatment_vals <- unname(quantile(sample_treatment, seq(0.05, 0.95, by = 0.3)))
  }
  
  results <- matrix(ncol = length(treatment_vals), nrow = M)
  ans <- data.frame(treatment_value = treatment_vals, ate = NA)
  vars <- names(dag); vars <- vars[vars != treatment]
  parents <- setNames(lapply(vars, function(var) dagitty::parents(dag, var)), vars) # TODO don't simulate detached nodes after removing treatment
  
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
