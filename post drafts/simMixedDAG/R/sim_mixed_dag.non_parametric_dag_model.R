sim_mixed_dag.non_parametric_dag_model <-
function(dag_model,  N = 1000, treatment_list = list()){
  dag <- dag_model$dag; gam_fits <- dag_model$gam_fits
  if(mean(names(gam_fits) %in% names(dag)) != 1 | length(names(dag)) != length(names(gam_fits))) stop("some variable entries in gam_fits don't match node names in supplied DAG")
  
  env <- environment()
  list2env(treatment_list, envir = env)
  vars <- names(dag)
  
  f_parentless <- function(sampling_vals, target_levels = NULL, N) {
    ans <- sample(x = sampling_vals[[1]], size = N, replace = T, prob = sampling_vals[[2]])
    if(!is.null(target_levels)) ans <- factor(ans, levels = target_levels)
    return(ans)
  }
  
  f_categorical <- function(gam_model, target_levels, parents, N, env){
    newdata <- list()
    for(parent in parents){
      newdata[[parent]] <- try(eval(parse(text = parent), envir = env), silent = T)
      if(class(newdata[[parent]]) == "try-error"){
        if(gam_fits[[parent]]$node_type == "parentless"){
          assign(parent, do.call(f_parentless, list(sampling_vals = gam_fits[[parent]]$sampling_vals, N = N, 
                                                    target_levels = gam_fits[[parent]]$target_levels), envir = env), pos = env) 
        } else if(gam_fits[[parent]]$node_type == "categorical") {
          assign(parent, do.call(f_categorical, list(gam_model = gam_fits[[parent]]$gam_model, 
                                                     parents = gam_fits[[parent]]$parents, 
                                                     target_levels = gam_fits[[parent]]$target_levels, 
                                                     N = N, 
                                                     env = env), envir = env), pos = env) 
        } else if(gam_fits[[parent]]$node_type == "continuous"){
          assign(parent, do.call(f_cont, list(gam_model = gam_fits[[parent]]$gam_model, 
                                              parents = gam_fits[[parent]]$parents, 
                                              target_levels = gam_fits[[parent]]$target_levels, 
                                              N = N, 
                                              env = env), envir = env), pos = env) 
        }
        newdata[[parent]] <- eval(parse(text = parent), envir = env)
      }
    }
    newdata <- as.data.frame(newdata) %>% mutate_all(list(~replace(., is.na(.), sample(x = ., size = sum(is.na(.)), replace = T)))) # this shouldn't be needed
    pred <- predict(gam_model, newdata, type = "response")
    if(length(target_levels) == 2) {
      return(target_levels[sapply(pred, function(p) rbinom(1, 1, p))])
    } else {
      return(factor(target_levels[apply(pred, 1, function(p_vec) which(rmultinom(1, 1, p_vec) == 1))], levels = target_levels))
    }
    
  }
  
  f_cont <- function(gam_model, parents, N, env){
    newdata <- list()
    for(parent in parents){
      newdata[[parent]] <- try(eval(parse(text = parent), envir = env), silent = T)
      if(class(newdata[[parent]]) == "try-error"){
        if(gam_fits[[parent]]$node_type == "parentless"){
          assign(parent, do.call(f_parentless, list(sampling_vals = gam_fits[[parent]]$sampling_vals, N = N, 
                                                    target_levels = gam_fits[[parent]]$target_levels), envir = env), pos = env) 
        } else if(gam_fits[[parent]]$node_type == "categorical") {
          assign(parent, do.call(f_categorical, list(gam_model = gam_fits[[parent]]$gam_model, 
                                                     parents = gam_fits[[parent]]$parents, 
                                                     target_levels = gam_fits[[parent]]$target_levels, 
                                                     N = N, 
                                                     env = env), envir = env), pos = env) 
        } else if(gam_fits[[parent]]$node_type == "continuous"){
          assign(parent, do.call(f_cont, list(gam_model = gam_fits[[parent]]$gam_model, 
                                              parents = gam_fits[[parent]]$parents, 
                                              N = N, 
                                              env = env), envir = env), pos = env) 
        }
        newdata[[parent]] <- eval(parse(text = parent), envir = env)
      }
    }
    newdata <- as.data.frame(newdata)
    return(as.vector(predict(gam_model, newdata, type = "response")) + rnorm(n = nrow(newdata), sd = gam_model$sd))
  }
  
  for(var in vars){
    if(!exists(var, envir = env)){ 
      if(gam_fits[[var]]$node_type == "parentless"){
        assign(var, do.call(f_parentless, list(sampling_vals = gam_fits[[var]]$sampling_vals, N = N, 
                                               target_levels = gam_fits[[var]]$target_levels)), pos = env) 
      } else if(gam_fits[[var]]$node_type == "categorical") {
        assign(var, do.call(f_categorical, list(gam_model = gam_fits[[var]]$gam_model, 
                                                parents = gam_fits[[var]]$parents, 
                                                target_levels = gam_fits[[var]]$target_levels, 
                                                N = N, 
                                                env = env)), pos = env) 
      } else if(gam_fits[[var]]$node_type == "continuous"){
        assign(var, do.call(f_cont, list(gam_model = gam_fits[[var]]$gam_model, 
                                         parents = gam_fits[[var]]$parents, 
                                         N = N, 
                                         env = env)), pos = env) 
      }
      
    }
  }
  return(setNames(data.frame(lapply(vars, function(var) eval(parse(text = var)))), vars))
}
