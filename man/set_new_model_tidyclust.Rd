% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa_models.R
\name{set_new_model_tidyclust}
\alias{set_new_model_tidyclust}
\alias{check_model_doesnt_exist_tidyclust}
\alias{set_model_mode_tidyclust}
\alias{check_model_exists_tidyclust}
\alias{set_model_engine_tidyclust}
\alias{set_dependency_tidyclust}
\alias{get_dependency_tidyclust}
\alias{set_fit_tidyclust}
\alias{get_fit_tidyclust}
\alias{get_encoding_tidyclust}
\alias{set_encoding_tidyclust}
\alias{set_model_arg_tidyclust}
\alias{show_model_info_tidyclust}
\alias{set_pred_tidyclust}
\alias{get_pred_type_tidyclust}
\title{Tools to Register Models}
\usage{
set_new_model_tidyclust(model)

check_model_doesnt_exist_tidyclust(model)

set_model_mode_tidyclust(model, mode)

check_model_exists_tidyclust(model)

set_model_engine_tidyclust(model, mode, eng)

set_dependency_tidyclust(model, eng, pkg = "tidyclust", mode = NULL)

get_dependency_tidyclust(model)

set_fit_tidyclust(model, mode, eng, value)

get_fit_tidyclust(model)

get_encoding_tidyclust(model)

set_encoding_tidyclust(model, mode, eng, options)

set_model_arg_tidyclust(model, eng, tidyclust, original, func, has_submodel)

show_model_info_tidyclust(model)

set_pred_tidyclust(model, mode, eng, type, value)

get_pred_type_tidyclust(model, type)
}
\arguments{
\item{model}{A single character string for the model type (e.g.
\code{"k_means"}, etc).}

\item{mode}{A single character string for the model mode (e.g. "partition").}

\item{eng}{A single character string for the model engine.}

\item{pkg}{An options character string for a package name.}

\item{value}{A list that conforms to the \code{fit_obj} or \code{pred_obj} description
below, depending on context.}

\item{options}{A list of options for engine-specific preprocessing encodings.
See Details below.}

\item{tidyclust}{A single character string for the "harmonized" argument name
that \code{tidyclust} exposes.}

\item{original}{A single character string for the argument name that
underlying model function uses.}

\item{func}{A named character vector that describes how to call
a function. \code{func} should have elements \code{pkg} and \code{fun}. The
former is optional but is recommended and the latter is
required. For example, \code{c(pkg = "stats", fun = "lm")} would be
used to invoke the usual linear regression function. In some
cases, it is helpful to use \code{c(fun = "predict")} when using a
package's \code{predict} method.}

\item{has_submodel}{A single logical for whether the argument
can make predictions on multiple submodels at once.}

\item{type}{A single character value for the type of prediction. Possible
values are: \code{cluster} and \code{raw}.}

\item{arg}{A single character string for the model argument name.}

\item{fit_obj}{A list with elements \code{interface}, \code{protect},
\code{func} and \code{defaults}. See the package vignette "Making a
\code{tidyclust} model from scratch".}

\item{pred_obj}{A list with elements \code{pre}, \code{post}, \code{func}, and \code{args}.}

\item{pre, post}{Optional functions for pre- and post-processing of prediction
results.}

\item{...}{Optional arguments that should be passed into the \code{args} slot for
prediction objects.}
}
\description{
These functions are similar to constructors and can be used to validate
that there are no conflicts with the underlying model structures used by the
package.
}
\details{
These functions are available for users to add their own models or
engines (in a package or otherwise) so that they can be accessed using
\code{tidyclust}.

In short, \code{tidyclust} stores an environment object that contains all of the
information and code about how models are used (e.g. fitting, predicting,
etc). These functions can be used to add models to that environment as well
as helper functions that can be used to makes sure that the model data is
in the right format.

\code{check_model_exists_tidyclust()} checks the model value and ensures that the
model has already been registered. \code{check_model_doesnt_exist_tidyclust()}
checks the model value and also checks to see if it is novel in the
environment.

The options for engine-specific encodings dictate how the predictors should
be handled. These options ensure that the data that \code{tidyclust} gives to the
underlying model allows for a model fit that is as similar as possible to
what it would have produced directly.

For example, if \code{fit()} is used to fit a model that does not have a formula
interface, typically some predictor preprocessing must be conducted.
\code{glmnet} is a good example of this.

There are four options that can be used for the encodings:

\code{predictor_indicators} describes whether and how to create indicator/dummy
variables from factor predictors. There are three options: \code{"none"} (do not
expand factor predictors), \code{"traditional"} (apply the standard
\code{model.matrix()} encodings), and \code{"one_hot"} (create the complete set
including the baseline level for all factors). This encoding only affects
cases when \code{\link[=fit.cluster_spec]{fit.cluster_spec()}} is used and the underlying model has an x/y
interface.

Another option is \code{compute_intercept}; this controls whether
\code{model.matrix()} should include the intercept in its formula. This affects
more than the inclusion of an intercept column. With an intercept,
\code{model.matrix()} computes dummy variables for all but one factor levels.
Without an intercept, \code{model.matrix()} computes a full set of indicators
for the \emph{first} factor variable, but an incomplete set for the remainder.

Next, the option \code{remove_intercept} will remove the intercept column
\emph{after} \code{model.matrix()} is finished. This can be useful if the model
function (e.g. \code{lm()}) automatically generates an intercept.

Finally, \code{allow_sparse_x} specifies whether the model function can natively
accommodate a sparse matrix representation for predictors during fitting
and tuning.
}
\examples{
# set_new_model_tidyclust("shallow_learning_model")

# Show the information about a model:
show_model_info_tidyclust("k_means")
}
\keyword{internal}
