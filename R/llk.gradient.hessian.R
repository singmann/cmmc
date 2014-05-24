
make.llk.function <- function(model, param.names, length.param.names) {
	model.character <- as.character(unlist(model))
	log.model.character <- lapply(model.character, function(x) paste("log(", x, ")", sep =""))
	data.log.model.character <- lapply(seq_along(model.character), function(x, log.model.character) paste("cmmc_data.", x, " * ", log.model.character[x], " + ", sep = ""), log.model.character = log.model.character)
	tmp.llk.character <- do.call("paste", as.list(data.log.model.character))
	llk.character <- substr(tmp.llk.character, 0, nchar(tmp.llk.character) - 3)
	parse(text = llk.character)
}

make.llk.gradient <- function(llk.function, param.names) {
	gradient.functions <- vector("list", length(param.names))
	for (param in seq_along(param.names)) {
		gradient.functions[[param]] <- D(llk.function, param.names[param])
	}
	gradient.functions
}



gradient_model_env2 <- function(model_environment) {
  function(par, data = NULL, ...){   
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = model_environment)
    else for (i in seq_along(model_environment[["parameter"]]))  assign(model_environment[["parameter"]][i], par[i], envir = model_environment)    
    if (is.null(data)) {
      data  <- rep(1, sum(model_environment[["n_item_type"]]))/rep(model_environment[["n_item_type"]], each = model_environment[["n_item_type"]])
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = model_environment)
    model.eval <- vapply(model_environment[["llk.gradient"]], eval, 0, envir = model_environment)
    model.eval[is.na(model.eval)] <- -1e10
    model.eval[model.eval == -Inf] <- -1e10
    return(-model.eval)
  }
}
  
hessian_model_env2 <- function(model_environment) {
  function(par, data = NULL, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = model_environment)
    else for (i in seq_along(model_environment[["parameter"]]))  assign(model_environment[["parameter"]][i], par[i], envir = model_environment)    
    if (is.null(data)) {
      data  <- rep(1, sum(model_environment[["n_item_type"]]))/rep(model_environment[["n_item_type"]], each = model_environment[["n_item_type"]])
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = model_environment)
    model.eval <- apply(model_environment[["llk.hessian"]], c(1,2), function(x) eval(x[[1]], envir = model_environment))
    model.eval[is.na(model.eval)] <- -1e10
    model.eval[model.eval == -Inf] <- -1e10
    return(-model.eval)
  }
}

make.llk.hessian <- function(llk.function, param.names) {
	gradient.functions <- make.llk.gradient(llk.function, param.names)
	hessian.functions <- vector("list", length(param.names)^2)
	for (llk.funct.outer in seq_along(gradient.functions)){
		for (llk.funct.inner in seq_along(gradient.functions)){
			hessian.functions[[((llk.funct.outer - 1) * length(param.names)) + llk.funct.inner]] <- D(gradient.functions[[llk.funct.outer]], param.names[llk.funct.inner])
		}
	}
	#tmp.out <- matrix(hessian.functions, length.param.names)
	#apply(tmp.out, c(1,2), function(x) parse(text = x[[1]]))
	matrix(hessian.functions, length(param.names))
}








predict_model2 <- function(model_environment) {
  function(par, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = model_environment)
    else for (i in seq_along(model_environment[["parameter"]]))  assign(model_environment[["parameter"]][i], par[i], envir = model_environment)
    vapply(model_environment[["unlist_model_list"]], eval, envir = model_environment, 0)
  }
}

llk_model2 <- function(model_environment) { 
  function(par, data = NULL, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = model_environment)
    else for (i in seq_along(model_environment[["parameter"]]))  assign(model_environment[["parameter"]][i], par[i], envir = model_environment)
    if (is.null(data)) {
      data  <- rep(1, sum(model_environment[["n_item_type"]]))/rep(model_environment[["n_item_type"]], each = model_environment[["n_item_type"]])
    }
    #browser()
    model.eval <- vapply(model_environment[["unlist_model_list"]], eval, envir = model_environment, 0)
    if (any(model.eval < 0, na.rm = TRUE)) stop(paste("Model not constructed well. Line ", which(model.eval < 0), " produces probabilities < 0!", sep = ""))
    llk <- data * log(model.eval)
    llk[data == 0] <- 0
    llk <- sum(llk)
    if (is.na(llk)) llk <- -1e10
    if (llk == -Inf) llk <- -1e10
    return(-llk)
  }
}

gradient_model2 <- function(llk.gradient, model_environment) {
  function(par, data = NULL, ...){   
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = model_environment)
    else for (i in seq_along(model_environment[["parameter"]]))  assign(model_environment[["parameter"]][i], par[i], envir = model_environment)    
    if (is.null(data)) {
      data  <- rep(1, sum(model_environment[["n_item_type"]]))/rep(model_environment[["n_item_type"]], each = model_environment[["n_item_type"]])
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = model_environment)
    model.eval <- vapply(llk.gradient, eval, 0, envir = model_environment)
    model.eval[is.na(model.eval)] <- -1e10
    model.eval[model.eval == -Inf] <- -1e10
    return(-model.eval)
  }
}
  
hessian_model2 <- function(llk.hessian, model_environment) {
  function(par, data = NULL, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = model_environment)
    else for (i in seq_along(model_environment[["parameter"]]))  assign(model_environment[["parameter"]][i], par[i], envir = model_environment)    
    if (is.null(data)) {
      data  <- rep(1, sum(model_environment[["n_item_type"]]))/rep(model_environment[["n_item_type"]], each = model_environment[["n_item_type"]])
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = model_environment)
    model.eval <- apply(llk.hessian, c(1,2), function(x) eval(x[[1]], envir = model_environment))
    model.eval[is.na(model.eval)] <- -1e10
    model.eval[model.eval == -Inf] <- -1e10
    return(-model.eval)
  }
}





predict_model <- function(model_list) {
  function(par, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = environment())
    else for (i in seq_along(model_list[["parameter"]]))  assign(model_list[["parameter"]][i], par[i], envir = environment())
    vapply(unlist(model_list[["model_list"]]), eval, envir = environment(), 0)
  }
}

llk_model <- function(model_list) { 
  function(par, data = NULL, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = environment())
    else for (i in seq_along(model_list[["parameter"]]))  assign(model_list[["parameter"]][i], par[i], envir = environment())
    if (is.null(data)) {
      n_item_type <- vapply(model_list[["model_list"]], length, 0)
      data  <- rep(1, sum(n_item_type))/rep(n_item_type, each = n_item_type)
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = environment())
    model.eval <- vapply(unlist(model_list[["model_list"]]), eval, envir = environment(), 0)
    if (any(model.eval < 0, na.rm = TRUE)) stop(paste("Model not constructed well. Line ", which(model.eval < 0), " produces probabilities < 0!", sep = ""))
    llk <- data * log(model.eval)
    llk[data == 0] <- 0
    llk <- sum(llk)
    if (is.na(llk)) llk <- -1e10
    if (llk == -Inf) llk <- -1e10
    return(-llk)
  }
}

gradient_model <- function(llk.gradient, model_list) {
  function(par, data = NULL, ...){   
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = environment())
    else for (i in seq_along(model_list[["parameter"]]))  assign(model_list[["parameter"]][i], par[i], envir = environment())    
    if (is.null(data)) {
      n_item_type <- vapply(model_list[["model_list"]], length, 0)
      data  <- rep(1, sum(n_item_type))/rep(n_item_type, each = n_item_type)
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = environment())
    model.eval <- vapply(llk.gradient, eval, 0, envir = environment())
    model.eval[is.na(model.eval)] <- -1e10
    model.eval[model.eval == -Inf] <- -1e10
    return(-model.eval)
  }
}
  
hessian_model <- function(llk.hessian, model_list) {
  function(par, data = NULL, ...){
    if (!is.null(names(par))) for (i in seq_along(par))  assign(names(par)[i], par[i], envir = environment())
    else for (i in seq_along(model_list[["parameter"]]))  assign(model_list[["parameter"]][i], par[i], envir = environment())    
    if (is.null(data)) {
      n_item_type <- vapply(model_list[["model_list"]], length, 0)
      data  <- rep(1, sum(n_item_type))/rep(n_item_type, each = n_item_type)
    }
    for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = environment())
    model.eval <- apply(llk.hessian, c(1,2), function(x) eval(x[[1]], envir = environment()))
    model.eval[is.na(model.eval)] <- -1e10
    model.eval[model.eval == -Inf] <- -1e10
    return(-model.eval)
  }
}