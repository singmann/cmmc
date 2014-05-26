
## virtual Classes needed for Slots:

setClassUnion("OptionalList", c("list", "NULL"))
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
