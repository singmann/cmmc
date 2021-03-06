% Generated by roxygen2 (4.0.1): do not edit by hand
\docType{class}
\name{CmmcMod-class}
\alias{CmmcMod-class}
\title{S4 class representing a model for categorical data}
\description{
This class is usually created by a call to \code{make_model}.
}
\details{
This class contains a objective, prediction, gradient, and hessian function of the model, a symbolic description of the model similar to the model equations, and some model info, such as parameter names and parameter bounds (upper and lower).
}
\section{Slots}{

\describe{
\item{\code{predict}}{\code{"function"} returning predictet proportion given parameter values.}

\item{\code{objective}}{\code{"function"} returning the likelihood given parameter values.}

\item{\code{gradient}}{\code{"function"} returning the analytical gradient given parameter values.}

\item{\code{hessian}}{\code{"function"} returning the analytical hessian given parameter values.}

\item{\code{model_environment}}{\code{"environment"} containing the model equations (likelihood, prediction, gradient, hessian), the parameters, and the data. Is used by all functions in this class.}

\item{\code{bounds}}{\code{"list"} of length 4 containing upper and lower bounds of parameters and starting values.}

\item{\code{model}}{\code{"list"} of length 2: \code{"parameter"} giving the name of the free parameters and \code{"model_list"} containing a list of models in which element is a \code{"list"} of \code{"expression"} corresponding to one item type.}

\item{\code{parameters_show}}{\code{"character"} vector given the parameters of the model to be displayed.}

\item{\code{model}}{\code{"list"} of restrictions.}
}}

