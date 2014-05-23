

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