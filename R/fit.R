
fit <- function(model, data, runs = 5, algorithm = "nlminb") {
  
  # control if arguments are present
  if(missing(data)) stop("data needs to be provided.")
  if(!is(model, "CmmcMod")) stop("model needs to be of class 'CmmcMod', as e.g., created by make_model().")
  
  browser()
  # get names from call:
  cl <- match.call()
  data_name <- as.character(cl["data"])
  model_name <- as.character(cl["model"])
  
  # check if data 
  if(is.vector(data)) {
    data <- array(data, dim = c(1, length(data)))
    multiFit <- FALSE
  } else
    if(dim(data)[1] == 1) {
      if (is.data.frame(data)) data <- as.matrix(data)
      data <- array(data, dim = c(1,length(data)))
      multiFit <- FALSE
    } else 
      if(is.matrix(data) | is.data.frame(data)) {
        if (is.data.frame(data)) data <- as.matrix(data)
        multiFit <- TRUE
      } else stop("data is neither vector, nor matrix, nor data.frame!")
  
}

get_start_values <- function(bounds, n) {
  mapply(function(x, y, n) runif(n, x, y), bounds$start_lower, bounds$start_upper, MoreArgs=list(n = n))
}

fit_nlminb <- function(model, data, start, use_gradient = TRUE, use_hessian = FALSE, control = list()) {
  tmp <- nlminb(start = start,
         objective = model@objective, 
         gradient = if (use_gradient) model@gradient else NULL,
         hessian = if (use_hessian) model@hessian else NULL,
         data = data,
         control = list(),
         lower = model@bounds$lower_bound,
         upper = model@bounds$upper_bound
         )
  list(
    parameter = tmp$par, 
    objective = tmp$objective, 
    convergence = if (tmp$convergence == 0) TRUE else FALSE,
    return = tmp)
}