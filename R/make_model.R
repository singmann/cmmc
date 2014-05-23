

make_model <- function(model, restrictions = NULL, bounds = NULL, starting_values = NULL, is_file = FALSE, is_file_restrictions = FALSE) {
  model_list <- make_model_list(model)
  predict <- predict_model(model_list)
  objective <- llk_model(model_list)
  likelihood <- tryCatch(make.llk.function(model_list[["model_list"]]), error = function(e) {warning("likelihood function cannot be build"); NULL})
  gradient <- tryCatch(gradient_model(make.llk.gradient(likelihood, model_list[["parameter"]]), model_list), error = function(e) {message("gradient function cannot be build (probably derivation failure, see ?D)\n Only numerical gradient available."); NULL})
  hessian <- tryCatch(hessian_model(make.llk.hessian(likelihood, model_list[["parameter"]]), model_list), error = function(e) {message("Hessian function cannot be build (probably derivation failure, see ?D)\n Only numerical Hessian available."); NULL})
  browser()
  par_sdt <- c(-1, -0.5, 0, 0.5, 1, 1, 1)
  objective(par_sdt)
  gradient(par_sdt)
  hessian(par_sdt)
  gradient(runif(3))
  hessian(runif(3))
  
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