# dag - an object of class "dagitty" representing the variable DAG
# g.args - named list. Each element contains arguments for the g function. The element name denotes the child node name. 
# Each element is a list with optional arguments:
## link: "identity"/"quadratic/cosin". This is the link function. 
## levels: positive integer. 1 indicates continuous. 2 and above indicate the number of levels in a categorical variable
## labels: categorical variable labels. Need to be have length equal to levels
## betas: a list. Each element name indicate parent node. Each element contains vector of beta coefficients
# N number of samples to generate

simMixedDAG <- function(dag, g.args,  N = 1000){
  g <- function(vars, link = "identity", levels = 1, betas = NULL, labels = NULL){
    if(!link %in% c("identity", "quadratic", "cosin")) stop("argument link in g.args has to be one of identity, quadratic, cosin")
    lp <- rep(0, N)
    for(i in seq_along(vars)){
      var_val <- try(eval(parse(text = vars[i]), envir = parent.frame(1L)), silent = T)
      if(class(var_val) == "try-error"){
        assign(vars[i], do.call(g, append(list(vars = parents[[vars[i]]]), g.args[[vars[i]]])), envir = pos.to.env(-1L))
        var_val <- eval(parse(text = vars[i]))
      }
      if(is.null(betas)){
        beta <- as.matrix(if(is.factor(var_val)) rnorm(length(levels(var_val))) else rnorm(1))
      } else {
        beta <- as.matrix(betas[[vars[i]]])
      }
      X <- model.matrix(~ var_val - 1)
      if(ncol(X) != nrow(beta)) stop(paste0("Number of levels in ", vars[i], " does not match number of input betas for that variable"))
      lp <- lp + as.numeric(X %*% beta)
    }
    
    if(link == "quadratic") lp <- sign(lp)*lp^2
    if(link == "cosin") lp <- cos(lp*2)
    
    lp <- lp + rnorm(length(lp)) # add noise
    lp <- (lp - mean(lp))/sd(lp) # standardize
    
    if(levels > 1){
      thresh <- qnorm(seq(0, 1, length.out = levels + 1))
      lp <- cut(lp, breaks = thresh, labels = if(is.null(labels)) LETTERS[1:levels] else labels, include.lowest = T)
    }
    return(lp)
  }
  
  parents <- setNames(lapply(names(dag), function(var) dagitty::parents(dag, var)), names(dag))
  vars <- names(parents)
  for(i in seq_along(names(parents))){
    if(!exists(vars[i])){
      assign(vars[i], do.call(g, append(list(vars = parents[[vars[i]]]), g.args[[vars[i]]])), envir = pos.to.env(-1L))
    }
  }
  return(setNames(data.frame(lapply(vars, function(var) eval(parse(text = var)))), vars))
}