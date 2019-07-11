non_parametric_dag_model <-
function(dag, data){
  if(mean(names(dag) %in% names(data)) != 1) stop("Some DAG nodes not found in data")
  if(any(is.na(data))) stop("data contains missing values")
  data <- data[names(dag)]
  data <- data %>% mutate_if(is.character, factor) %>% 
    mutate_if(function(x) length(unique(x)) == 2, factor) # code 2 value variables as binary
  vars <- names(dag)
  ans <- setNames(object = vector(mode = "list", length = length(vars)), nm = vars)
  
  for(var in vars){
    var_parents <- parents(dag, var)
    if(length(var_parents) == 0){
      ans[[var]]$node_type <- "parentless"
      sampling_vals <- tapply(data[[var]], data[[var]], length)/length(data[[var]])
      sampling_vals <- data.frame(value = names(sampling_vals), freq = sampling_vals, row.names = NULL, stringsAsFactors = F)
      if(is.numeric(data[[var]])) sampling_vals$value = as.numeric(sampling_vals$value)
      ans[[var]]$sampling_vals <- sampling_vals
      ans[[var]]$target_levels <- if(is.factor(data[[var]])) levels(data[[var]]) else NULL
    } else {
      ans[[var]]$parents <- var_parents
      if(class(data[[var]]) == "factor") {
        ans[[var]]$node_type <- "categorical"
        forms <- lapply(unlist(strsplit(paste0(var, paste0(rep(paste0(" ~ ", paste0(sapply(var_parents, function(var_parent) {
          if(class(data[[var_parent]]) == "factor") return(var_parent) else return(paste0("s(", var_parent, ")"))
        }), collapse = " + ")), length(levels(data[[var]])) - 1), collapse = ", ")), ",")), as.formula)
        
        ans[[var]]$target_levels <- levels(data[[var]])
        num_levels <- length(levels(data[[var]]))
        dat <- data[c(var, var_parents)]; dat[[var]] <- as.integer(dat[[var]]) - 1
        ans[[var]]$gam_model <- gam(forms, family = if(num_levels == 2) "binomial" else multinom(K = num_levels - 1), data = dat)
      } else {
        ans[[var]]$node_type <- "continuous"
        form <- as.formula(paste0(var, " ~ ", paste0(sapply(var_parents, function(var_parent) {
          if(class(data[[var_parent]]) == "factor") return(var_parent) else return(paste0("s(", var_parent, ")"))
        }), collapse = " + ")))
        dat <- data[c(var, var_parents)]
        ans[[var]]$gam_model <- gam(formula = form, family = "gaussian", data = dat)
        ans[[var]]$gam_model$sd <- sd(dat[[var]])
      }
    }
  }
  ans <- list(dag = dag, gam_fits = ans)
  class(ans) <- "non_parametric_dag_model"
  return(ans)
}
