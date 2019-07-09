# dag - an object of class "dagitty" representing the variable DAG
# f.args - named list. Each element contains arguments for the g function. The element name denotes the child node name. 
# Each element is a list with optional arguments:
## link: "identity"/"quadratic/cosin". This is the link function. 
## levels: positive integer. 1 indicates continuous. 2 and above indicate the number of levels in a categorical variable
## betas: a list. Each element name indicate parent node. Each element contains beta coefficients (scalar for numeric parent, vector of length levles - 1 for factor with the firstt level being the baseline)
## labels: categorical variable labels. Need to be have length equal to the levels argument
## sinr: signal to noise ratio
# n number of samples to generate
## ... set variables using this
library(gam); library(mgcv)

f_parentless <- function(sampling_vals, n, target_levels = NULL) {
  ans <- sample(x = sampling_vals[[1]], size = n, replace = T, prob = sampling_vals[[2]])
  if(!is.null(target_levels)) ans <- factor(ans, levels = target_levels)
  return(ans)
}

f_categorical <- function(gam_model, target_levels, parents, n, env){
  newdata <- list()
  for(parent in parents){
    newdata[[parent]] <- try(eval(parse(text = parent), envir = env), silent = T)
    if(class(newdata[[parent]]) == "try-error"){
      if(gam_fits[[parent]]$node_type == "parentless"){
        assign(parent, do.call(f_parentless, list(sampling_vals = gam_fits[[parent]]$sampling_vals, n = n, 
                                                  target_levels = gam_fits[[parent]]$target_levels)), pos = env) 
      } else if(gam_fits[[parent]]$node_type == "categorical") {
        assign(parent, do.call(f_categorical, list(gam_model = gam_fits[[parent]]$gam_model, 
                                                   parents = gam_fits[[parent]]$parents, 
                                                   target_levels = gam_fits[[parent]]$target_levels, 
                                                   n = n, 
                                                   env = env)), pos = env) 
      } else if(gam_fits[[parent]]$node_type == "continuous"){
        assign(parent, do.call(f_cont, list(gam_model = gam_fits[[parent]]$gam_model, 
                                            parents = gam_fits[[parent]]$parents, 
                                            target_levels = gam_fits[[parent]]$target_levels, 
                                            n = n, 
                                            env = env)), pos = env) 
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

f_cont <- function(gam_model, parents, n, env){
  newdata <- list()
  for(parent in parents){
    newdata[[parent]] <- try(eval(parse(text = parent), envir = env), silent = T)
    if(class(newdata[[parent]]) == "try-error"){
      if(gam_fits[[parent]]$node_type == "parentless"){
        assign(parent, do.call(f_parentless, list(sampling_vals = gam_fits[[parent]]$sampling_vals, n = n, 
                                                  target_levels = gam_fits[[parent]]$target_levels)), pos = env) 
      } else if(gam_fits[[parent]]$node_type == "categorical") {
        assign(parent, do.call(f_categorical, list(gam_model = gam_fits[[parent]]$gam_model, 
                                                   parents = gam_fits[[parent]]$parents, 
                                                   target_levels = gam_fits[[parent]]$target_levels, 
                                                   n = n, 
                                                   env = env)), pos = env) 
      } else if(gam_fits[[parent]]$node_type == "continuous"){
        assign(parent, do.call(f_cont, list(gam_model = gam_fits[[parent]]$gam_model, 
                                            parents = gam_fits[[parent]]$parents, 
                                            n = n, 
                                            env = env)), pos = env) 
      }
      newdata[[parent]] <- eval(parse(text = parent), envir = env)
    }
  }
  newdata <- as.data.frame(newdata)
  return(as.vector(predict(gam_model, newdata, type = "response")) + rnorm(n = nrow(newdata), sd = gam_model$sd))
}

fit_gam <- function(dag, data){
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
  return(ans)
}
