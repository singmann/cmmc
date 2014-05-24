

make_model <- function(model, restrictions = NULL, bounds = NULL, starting_values = NULL, is_file = FALSE, is_file_restrictions = FALSE) {
  # read model:
  model_list <- make_model_list(model)
  # parameters that will be dsiplayed in the output:
  parameters_show <- model_list[["parameter"]]
  # handle restrictions:
  if (!is.null(restrictions)) {
    NULL # restriction handling here
    # make new model_list
  }
  ## local variables for easy programming:
  n_parameter <- length(model_list[["parameter"]])
  
  # make functions (prediction, likelihood, ...)
  predict <- predict_model(model_list)
  objective <- llk_model(model_list)
  likelihood <- tryCatch(make.llk.function(model_list[["model_list"]]), error = function(e) {warning("likelihood function cannot be build"); NULL})
  gradient <- tryCatch(gradient_model(make.llk.gradient(likelihood, model_list[["parameter"]]), model_list), error = function(e) {message("gradient function cannot be build (probably derivation failure, see ?D)\n Only numerical gradient available."); NULL})
  hessian <- tryCatch(hessian_model(make.llk.hessian(likelihood, model_list[["parameter"]]), model_list), error = function(e) {message("Hessian function cannot be build (probably derivation failure, see ?D)\n Only numerical Hessian available."); NULL})
  
  # create bounds:
  if (is.null(bounds)) {
    bounds <- list(
      lower_bound = rep(0, n_parameter),
      upper_bound = rep(1, n_parameter)
      )
  }
  if (is.null(starting_values)) {
    starting_values <- list(
      start_lower = rep(0.1, n_parameter),
      start_upper = rep(0.9, n_parameter)
      )
  }
  
  # return CmmcMod object:
  new("CmmcMod",
      predict = predict,
      objective = objective,
      gradient = gradient,
      hessian = hessian,
      model = model_list,
      bounds = c(bounds, starting_values),
      parameters_show = parameters_show,
      restrictions = NULL      
      )
}


make_model_cmp <- function(model, restrictions = NULL, bounds = NULL, starting_values = NULL, is_file = FALSE, is_file_restrictions = FALSE) {
  # read model:
  model_list <- make_model_list(model)
  # parameters that will be dsiplayed in the output:
  parameters_show <- model_list[["parameter"]]
  # handle restrictions:
  if (!is.null(restrictions)) {
    NULL # restriction handling here
    # make new model_list
  }
  ## local variables for easy programming:
  n_parameter <- length(model_list[["parameter"]])
  
  # make functions (prediction, likelihood, ...)
  predict <- predict_model(model_list)
  objective <- llk_model(model_list)
  likelihood <- tryCatch(make.llk.function(model_list[["model_list"]]), error = function(e) {warning("likelihood function cannot be build"); NULL})
  gradient <- tryCatch(gradient_model(make.llk.gradient(likelihood, model_list[["parameter"]]), model_list), error = function(e) {message("gradient function cannot be build (probably derivation failure, see ?D)\n Only numerical gradient available."); NULL})
  hessian <- tryCatch(hessian_model(make.llk.hessian(likelihood, model_list[["parameter"]]), model_list), error = function(e) {message("Hessian function cannot be build (probably derivation failure, see ?D)\n Only numerical Hessian available."); NULL})
  
  # create bounds:
  if (is.null(bounds)) {
    bounds <- list(
      lower_bound = rep(0, n_parameter),
      upper_bound = rep(1, n_parameter)
      )
  }
  if (is.null(starting_values)) {
    starting_values <- list(
      start_lower = rep(0.1, n_parameter),
      start_upper = rep(0.9, n_parameter)
      )
  }
  
  # return CmmcMod object:
  new("CmmcMod",
      predict = compiler::cmpfun(predict),
      objective = compiler::cmpfun(objective),
      gradient = compiler::cmpfun(gradient),
      hessian = compiler::cmpfun(hessian),
      model = model_list,
      bounds = c(bounds, starting_values),
      parameters_show = parameters_show,
      restrictions = NULL      
      )
}




make_model_env_cmp <- function(model, restrictions = NULL, bounds = NULL, starting_values = NULL, is_file = FALSE, is_file_restrictions = FALSE) {
  # read model:
  model_list <- make_model_list(model)
  # parameters that will be dsiplayed in the output:
  parameters_show <- model_list[["parameter"]]
  # handle restrictions:
  if (!is.null(restrictions)) {
    NULL # restriction handling here
    # make new model_list
  }
  ## local variables for easy programming:
  n_parameter <- length(model_list[["parameter"]])
  
  model_environment <- new.env()
  #browser()
  #assign("model_list", model_list[["model_list"]], envir=model_environment)
  assign("unlist_model_list", unlist(model_list[["model_list"]]), envir=model_environment)
  assign("parameter", model_list[["parameter"]], envir=model_environment)
  assign("length_parameter", length(model_list[["parameter"]]), envir=model_environment)
  assign("n_item_type", vapply(model_list[["model_list"]], length, 0), envir=model_environment)
  
  #n_item_type <- vapply(model_list[["model_list"]], length, 0)
  #data  <- rep(1, sum(n_item_type))/rep(n_item_type, each = n_item_type)
  
  #ls.str(envir = model_environment)
  # make functions (prediction, likelihood, ...)
  predict <- predict_model2(model_environment)
  objective <- llk_model2(model_environment)
  likelihood <- tryCatch(make.llk.function(model_list[["model_list"]]), error = function(e) {warning("likelihood function cannot be build"); NULL})
  gradient <- tryCatch(gradient_model2(make.llk.gradient(likelihood, model_list[["parameter"]]), model_environment), error = function(e) {message("gradient function cannot be build (probably derivation failure, see ?D)\n Only numerical gradient available."); NULL})
  hessian <- tryCatch(hessian_model2(make.llk.hessian(likelihood, model_list[["parameter"]]), model_environment), error = function(e) {message("Hessian function cannot be build (probably derivation failure, see ?D)\n Only numerical Hessian available."); NULL})
  
  # create bounds:
  if (is.null(bounds)) {
    bounds <- list(
      lower_bound = rep(0, n_parameter),
      upper_bound = rep(1, n_parameter)
      )
  }
  if (is.null(starting_values)) {
    starting_values <- list(
      start_lower = rep(0.1, n_parameter),
      start_upper = rep(0.9, n_parameter)
      )
  }
  
  # return CmmcMod object:
  new("CmmcModEnv",
      predict = compiler::cmpfun(predict),
      objective = compiler::cmpfun(objective),
      gradient = compiler::cmpfun(gradient),
      hessian = compiler::cmpfun(hessian),
      model_environment = model_environment,
      model = model_list,
      bounds = c(bounds, starting_values),
      parameters_show = parameters_show,
      restrictions = NULL      
      )
}



make_model_env <- function(model, restrictions = NULL, bounds = NULL, starting_values = NULL, is_file = FALSE, is_file_restrictions = FALSE) {
  # read model:
  model_list <- make_model_list(model)
  # parameters that will be dsiplayed in the output:
  parameters_show <- model_list[["parameter"]]
  # handle restrictions:
  if (!is.null(restrictions)) {
    NULL # restriction handling here
    # make new model_list
  }
  ## local variables for easy programming:
  n_parameter <- length(model_list[["parameter"]])
  
  model_environment <- new.env()
  #browser()
  #assign("model_list", model_list[["model_list"]], envir=model_environment)
  assign("unlist_model_list", unlist(model_list[["model_list"]]), envir=model_environment)
  assign("parameter", model_list[["parameter"]], envir=model_environment)
  assign("length_parameter", length(model_list[["parameter"]]), envir=model_environment)
  assign("n_item_type", vapply(model_list[["model_list"]], length, 0), envir=model_environment)
  
  #n_item_type <- vapply(model_list[["model_list"]], length, 0)
  #data  <- rep(1, sum(n_item_type))/rep(n_item_type, each = n_item_type)
  
  #ls.str(envir = model_environment)
  # make functions (prediction, likelihood, ...)
  predict <- predict_model2(model_environment)
  objective <- llk_model2(model_environment)
  likelihood <- tryCatch(make.llk.function(model_list[["model_list"]]), error = function(e) {warning("likelihood function cannot be build"); NULL})
  gradient <- tryCatch(gradient_model2(make.llk.gradient(likelihood, model_list[["parameter"]]), model_environment), error = function(e) {message("gradient function cannot be build (probably derivation failure, see ?D)\n Only numerical gradient available."); NULL})
  hessian <- tryCatch(hessian_model2(make.llk.hessian(likelihood, model_list[["parameter"]]), model_environment), error = function(e) {message("Hessian function cannot be build (probably derivation failure, see ?D)\n Only numerical Hessian available."); NULL})
  
  # create bounds:
  if (is.null(bounds)) {
    bounds <- list(
      lower_bound = rep(0, n_parameter),
      upper_bound = rep(1, n_parameter)
      )
  }
  if (is.null(starting_values)) {
    starting_values <- list(
      start_lower = rep(0.1, n_parameter),
      start_upper = rep(0.9, n_parameter)
      )
  }
  
  # return CmmcMod object:
  new("CmmcModEnv",
      predict = predict,
      objective = objective,
      gradient = gradient,
      hessian = hessian,
      model_environment = model_environment,
      model = model_list,
      bounds = c(bounds, starting_values),
      parameters_show = parameters_show,
      restrictions = NULL      
      )
}


make_model_env2_cmp <- function(model, restrictions = NULL, bounds = NULL, starting_values = NULL, is_file = FALSE, is_file_restrictions = FALSE) {
  # read model:
  model_list <- make_model_list(model)
  # parameters that will be dsiplayed in the output:
  parameters_show <- model_list[["parameter"]]
  # handle restrictions:
  if (!is.null(restrictions)) {
    NULL # restriction handling here
    # make new model_list
  }
  ## local variables for easy programming:
  n_parameter <- length(model_list[["parameter"]])
  
  model_environment <- new.env()
  #browser()
  #assign("model_list", model_list[["model_list"]], envir=model_environment)
  assign("unlist_model_list", unlist(model_list[["model_list"]]), envir=model_environment)
  assign("parameter", model_list[["parameter"]], envir=model_environment)
  assign("length_parameter", length(model_list[["parameter"]]), envir=model_environment)
  assign("n_item_type", vapply(model_list[["model_list"]], length, 0), envir=model_environment)
  
  #n_item_type <- vapply(model_list[["model_list"]], length, 0)
  #data  <- rep(1, sum(n_item_type))/rep(n_item_type, each = n_item_type)
  
  #ls.str(envir = model_environment)
  # make functions (prediction, likelihood, ...)
  predict <- predict_model2(model_environment)
  objective <- llk_model2(model_environment)
  likelihood <- tryCatch(make.llk.function(model_list[["model_list"]]), error = function(e) {warning("likelihood function cannot be build, please report example."); NULL})  
  assign("llk.gradient", tryCatch(make.llk.gradient(likelihood, model_list[["parameter"]]), error = function(e) {message("gradient function cannot be build (probably derivation failure, see ?D)\n Only numerical gradient available."); NULL}), envir=model_environment)
  assign("llk.hessian", tryCatch(make.llk.hessian(likelihood, model_list[["parameter"]]), error = function(e) {message("Hessian function cannot be build (probably derivation failure, see ?D)\n Only numerical Hessian available."); NULL}), envir=model_environment)
  
  gradient <- if (!is.null(model_environment[["llk.gradient"]])) gradient_model_env2(model_environment) else NULL
  hessian <- if (!is.null(model_environment[["llk.hessian"]])) hessian_model_env2(model_environment) else NULL
  
  # create bounds:
  if (is.null(bounds)) {
    bounds <- list(
      lower_bound = rep(0, n_parameter),
      upper_bound = rep(1, n_parameter)
      )
  }
  if (is.null(starting_values)) {
    starting_values <- list(
      start_lower = rep(0.1, n_parameter),
      start_upper = rep(0.9, n_parameter)
      )
  }
  
  # return CmmcMod object:
  new("CmmcModEnv",
      predict = compiler::cmpfun(predict),
      objective = compiler::cmpfun(objective),
      gradient = compiler::cmpfun(gradient),
      hessian = compiler::cmpfun(hessian),
      model_environment = model_environment,
      model = model_list,
      bounds = c(bounds, starting_values),
      parameters_show = parameters_show,
      restrictions = NULL      
      )
}

# makes model element of CmmcMod
make_model_list <- function(model) {
  model_list <- read_model(model)
  parameters <- unique(sort(unique(unlist(lapply(unlist(model_list), all.vars)))))
  c(parameter = list(parameters), model_list = list(model_list))
}

# read_model() reads a model (as text),
# splits the string into characters for each row,
# and then parses it into a list of code elements.
read_model <- function(model) {
  whole <- strsplit(model, "[\n\r]")[[1]] # split character string into single lines.
  whole <- gsub("#.*", "", whole) # remove comments
  model <- vector("list", length(whole))
	c2 <- 1
	c3 <- 1
	s.flag <- FALSE
	for (c1 in 1:length(whole)) {
		if (!(grepl("^[[:space:]]*$", whole[c1]))) {  # if empty line, use next list
			s.flag <- TRUE
			model[[c2]][c3] <- parse(text = whole[c1])[1]
			c3 <- c3 + 1
			fin <- c2
		}
		else {
			if (s.flag == TRUE) c2 <- c2 + 1
			c3 <- 1
			s.flag <- FALSE
		}
	}
	model[1:fin]
}