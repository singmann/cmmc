
fit <- function(model, data, runs = 5,aggregated = TRUE,
                use_gradient = TRUE, use_hessian = FALSE, refit = TRUE,
                method = "nlminb", control = list()) {
  
  # control if arguments are present
  if(missing(data)) stop("data needs to be provided.")
  if(!is(model, "CmmcMod")) stop("model needs to be of class 'CmmcMod', as e.g., created by make_model().")
  
  # get names from call:
  cl <- match.call()
  data_name <- as.character(cl["data"])
  model_name <- as.character(cl["model"])
  
  # check data (and potentially transform to matrix)
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
  ndata <- nrow(data)
  if (multiFit) {
    if (!is.null(rownames(data))) data_names <- rownames(data)
    else data_names <- paste0(data_name, "_", seq_len(ndata))
    if (aggregated) {
      data <- rbind(data, colSums(data))
      ndata <- nrow(data)
      data_names <- c(data_names, "aggregated")
    }
  } else data_names <- data_name
  rownames(data) <- data_names
  
  # check method and set to function
  method <- match.arg(method, choices=c("nlminb", "optimx"))
  if (method == "nlminb") method <- fit_nlminb 
  else if (method == "optimx") method <- fit_optimx
  
  fitted <- Cmmc$new(
    model = model,
    data = data,
    names = c(model_name, data_name),
    optinfo = list(
      optima = NULL,
      five_best = list(),
      refit = refit
      )
    )
  ## obtain first fits:
  starting_values <- lapply(seq_len(ndata), function(x) get_start_values(bounds = model@bounds, n = runs))
  if (is.null(dim(starting_values[[1]]))) starting_values <- lapply(starting_values, function(x) t(as.matrix(x)))
  
  obtain_individual_fits <- function(fitted, runs, starting_values, use_gradient, use_hessian, method, control) {
    lapply(seq_len(runs), 
           function(x) do.call(what = method, 
                               args = list(model = fitted$model,
                                           start = starting_values[x,], 
                                           use_gradient = use_gradient, use_hessian = use_hessian,
                                           control = control))
    )
  }
  #method(model = fitted$model,start = starting_values[[1]][[1]], use_gradient = use_gradient, use_hessian = use_hessian, control = control)
  
  obtain_fits <- function(y) {
    assign("data", data[y,], envir = fitted$model@model_environment)
    for (d in seq_along(data[y,])) assign(paste("cmmc_data.", d, sep = ""), data[y,d], envir = fitted$model@model_environment)
    tmp <- obtain_individual_fits(fitted = fitted, runs = runs, starting_values[[y]], use_gradient = use_gradient, use_hessian = use_hessian, method = method, control = control)
    list(parameter = lapply(tmp, "[[", i = "parameter"), 
         df = data.frame(value = vapply(tmp, "[[", 0, i = "value"), 
                         convergence = vapply(tmp, "[[", NA, i = "convergence"),
                         use_gradient = vapply(tmp, "[[", NA, i = "use_gradient"), 
                         use_hessian = vapply(tmp, "[[", NA, i = "use_hessian"), 
                         method = vapply(tmp, "[[", "", i = "method"),
                         message = vapply(tmp, "[[", "", i = "message")),
         returned = lapply(tmp, "[[", i = "returned"))
  }
  
  fits <- lapply(seq_len(ndata), obtain_fits)
  optima_ids <- vapply(fits, function(x) which(x[["df"]]$value == min(x[["df"]]$value))[1], -1)
  optima_df <- do.call("rbind", mapply(function(x, y) x[[2]][y,], fits, optima_ids, SIMPLIFY=FALSE))
  optima_par <- do.call("rbind", mapply(function(x, y) x[[1]][[y]], fits, optima_ids, SIMPLIFY=FALSE))
  
  # obtain second fits if necessary
  if (refit && any(!optima_df$convergence)) {
    to_refit <- which(!optima_df$convergence)
    ## to do!!
  }
  rownames(optima_df) <- data_names
  fitted$optinfo$optima <- optima_df
  fitted$optinfo$five_best <- lapply(fits, function(x) {
    select <- order(x$df$value)[1:5]
    list(parameters = x[[1]][select], df = x[[2]][select,], returned = x[[3]][select])
  })
  
  fitted$gof <- data.frame(log.likelihood = -optima_df[, "value"],  g = vapply(seq_len(ndata), function(x, data, Log.Likelihood) as.numeric(2*(Log.Likelihood[x]-sat_model(fitted$model@model[["model_list"]], data[x,,drop = FALSE]))), 0, data = data, Log.Likelihood = optima_df[, "value"]), df = .DF.N.get(data[1,, drop = FALSE], model_list = fitted$model@model[["model_list"]])[[1]])
  rownames(fitted$gof) <- data_names
  
  rownames(optima_par) <- data_names
  colnames(optima_par) <- fitted$model@model[["parameter"]]
  
  fitted$coef <- optima_par
  
  fitted$vcov <- array(as.numeric(NA), dim = c(rep(ncol(optima_par),2), ndata), 
                       dimnames = list(
                         fitted$model@model[["parameter"]],
                         fitted$model@model[["parameter"]],
                         data_names
                         ))
  for (i in seq_len(ndata)) {
    assign("data", data[i,], envir = fitted$model@model_environment)
    for (d in seq_along(data[i,])) assign(paste("cmmc_data.", d, sep = ""), data[i,d], envir = fitted$model@model_environment)
    fitted$vcov[,,i] <- tryCatch(solve(fitted$model@hessian(fitted$coef[i,])), error = function(e) NA)
  }
  #browser()
  if (multiFit) {
    if (aggregated) {
      CmmcMulti$new(
        model = model,
        data = data[-ndata,],
        names = c(model_name, data_name),
        gof = fitted$gof[-ndata,],
        coef = fitted$coef[-ndata,],
        vcov = fitted$vcov[,,-ndata],
        optinfo = list(
          optima = fitted$optinfo$optima[-ndata,],
          five_best = fitted$optinfo$five_best[-ndata],
          refit = refit
        ),
        aggregated_data = aggregated,
        aggregated = Cmmc$new(
          model = model,
          data = data[ndata,,drop = FALSE],
          names = c(model_name, data_name),
          gof = fitted$gof[ndata,,drop = FALSE],
          coef = fitted$coef[ndata,,drop = FALSE],
          vcov = fitted$vcov[,,ndata,drop = FALSE],
          optinfo = list(
            optima = fitted$optinfo$optima[ndata,],
            five_best = fitted$optinfo$five_best[ndata],
            refit = refit
          )
        )
      )
    } else {
      CmmcMulti$new(
        fitted,
        aggregated_data = FALSE,
        aggregated = NULL
      )
    }
  } else fitted  
}

get_start_values <- function(bounds, n) {
  mapply(function(x, y, n) runif(n, x, y), bounds$start_lower, bounds$start_upper, MoreArgs=list(n = n), SIMPLIFY=TRUE)
}

fit_nlminb <- function(model, start, use_gradient = TRUE, use_hessian = FALSE, control = list()) {
  #assign("data", data, envir = model@model_environment)
  #for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = model@model_environment)
  tmp <- nlminb(start = start,
         objective = model@objective,
         gradient = if (use_gradient) model@gradient else NULL,
         hessian = if (use_hessian) model@hessian else NULL,
         control = control,
         lower = model@bounds$lower_bound,
         upper = model@bounds$upper_bound
         )
  list(
    parameter = tmp$par,
    value = tmp$objective,
    convergence = if (tmp$convergence == 0) TRUE else FALSE,
    use_gradient = use_gradient,
    use_hessian = use_hessian,
    method = "nlminb",
    message = tmp$message,
    returned = tmp
    )
}

fit_optimx <- function(model, start, use_gradient = TRUE, use_hessian = FALSE, control = list()) {
  #assign("data", data, envir = model@model_environment)
  #for (d in seq_along(data)) assign(paste("cmmc_data.", d, sep = ""), data[d], envir = model@model_environment)
  #browser()
  if (length(control) == 0L) {
    control <- list(all.methods = TRUE)
    method  <- ""
  } else if (!("method" %in% names(control))) {
    control <- c(all.methods = TRUE, c(control))
    method  <- ""
  } else {
    method <- control[["method"]]
    control <- control[-(which(names(control) == "method"))]
  }
  tmp <- optimx(par = start,
         fn = model@objective,
         gr = if (use_gradient) model@gradient else NULL,
         hess = if (use_hessian) model@hessian else NULL,
         method = method,
         control = control,
         lower = model@bounds$lower_bound,
         upper = model@bounds$upper_bound
         )
  if (nrow(tmp) == 1) best <- tmp
  else best <- tmp[order(tmp$value)[1],]
  list(
    parameter = unlist(best[,grep("^p", colnames(tmp))]),
    value = best$value,
    convergence = if (best$convcode == 0) TRUE else FALSE,
    use_gradient = use_gradient,
    use_hessian = use_hessian,
    method = rownames(best)[1],
    message = paste0("kkt1: ", best$kkt1, "; kkt2: ", best$kkt2),
    returned = tmp
    )
}
