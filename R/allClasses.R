
## virtual Classes needed for Slots:

setClassUnion("OptionalList", c("list", "NULL"))
# setClassUnion("OptionalMatrix", c("matrix", "NULL"))
# setClassUnion("OptionalArray", c("array", "NULL"))
# setClassUnion("OptionalData.frame", c("data.frame", "NULL"))
#setClassUnion("OptionalFunction", c("function", "NULL"))

## Restriction

setClass("Restriction", 
         slots = list(
           restricted = "character",
           to = "character",
           type = "character",
           literal = "character"))

### CmmcMod

#' S4 class representing a model for categorical data
#' 
#' This class is usually created by a call to \code{make_model}.
#' 
#' This class contains a objective, prediction, gradient, and hessian function of the model, a symbolic description of the model similar to the model equations, and some model info, such as parameter names and parameter bounds (upper and lower).
#' 
#' @slot predict \code{"function"} returning predictet proportion given parameter values.
#' @slot objective \code{"function"} returning the likelihood given parameter values.
#' @slot gradient \code{"function"} returning the analytical gradient given parameter values.
#' @slot hessian \code{"function"} returning the analytical hessian given parameter values.
#' @slot model_environment \code{"environment"} containing the model equations (likelihood, prediction, gradient, hessian), the parameters, and the data. Is used by all functions in this class.
#' @slot bounds \code{"list"} of length 4 containing upper and lower bounds of parameters and starting values.
#' @slot model \code{"list"} of length 2: \code{"parameter"} giving the name of the free parameters and \code{"model_list"} containing a list of models in which element is a \code{"list"} of \code{"expression"} corresponding to one item type.
#' @slot parameters_show \code{"character"} vector given the parameters of the model to be displayed.
#' @slot model \code{"list"} of restrictions.
#' @export
setClass("CmmcMod", 
         slots = list(
           predict = "function",
           objective = "function",
           gradient = "OptionalFunction",
           hessian = "OptionalFunction",
           model_environment = "environment",
           bounds = "list",
           model = "list",
           parameters_show = "character",
           restrictions = "OptionalList"))



Cmmc <- setRefClass("Cmmc", 
                    fields = list(
                      model = "CmmcMod",
                      data = "matrix",
                      gof = "data.frame",
                      coef = "matrix",
                      vcov = "array",
                      optinfo = "list",
                      names = "character"
                    ),
                    methods = list(
                      show = function(show_info = TRUE, show_data_message = TRUE, extra = NULL) {
                        cat_per_type <- vapply(.self$model@model$model_list, length, 0)
                        if (show_info) {
                          cat("Fit of cmmc model '", .self$names[1], "' to '", .self$names[2], "'.\n", sep = "")                          
                          cat(length(.self$model@model$model_list), "item type(s) with", min(cat_per_type), "to", max(cat_per_type), "categories/type. Total n =", sum(data), "\n\n")
                        }
                        if (!is.null(extra)) cat(extra, "\n", sep = "")
                        cat("Goodness of fit:\n")
                        t_gof <- colSums(.self$gof)
                        t_gof <- c(t_gof, p = unname(pchisq(t_gof[2], t_gof[3], lower.tail=FALSE)))
                        names(t_gof)[1:2] <- c("logLik", "G^2")
                        print.default(prettyNum(t_gof, digits = 3), print.gap = 2L, quote = FALSE)
                        cat("Parameters:\n")
                        print.default(format(colMeans(.self$coef), digits = 3), print.gap = 2L, quote = FALSE)
                        if (!is.null(extra)) cat("\n")
                        if (show_data_message && nrow(.self$data) > 1) message("nrow(data) > 1, displaying summed/mean values.")
                      }
                    )
)

setClassUnion("OptionalCmmc", c("Cmmc", "NULL"))

CmmcMulti <- setRefClass("CmmcMulti", 
                         contains= "Cmmc",
                         fields = list(
                           aggregated_data = "logical",
                           aggregated = "OptionalCmmc"
                         ),
                         methods = list(
                           show = function() {
                             callSuper(show_data_message = FALSE, extra = "Summed/Mean values")
                             if (.self$aggregated_data) .self$aggregated$show(show_info = FALSE, extra = "Aggregated data:")
                             #browser()
                           }
                         )
)