% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{projConfig}
\alias{projConfig}
\title{Project configuration class}
\description{
Project configuration class

Project configuration class
}
\note{
Several fields use a update their list values when assigning to them,
rather than replacing the entire list as might be expected with traditional R assignment.
This can make updating lists easier (don't need to worry about copy existing values
or otherwise worry about appending/updating the list; or accidentally dropping elements),
and allows for checking or validation of values upon assignment.
Thus, the user should assign a list containing the (named) sub-element to be updated rather
than attempting to assign to a sub-element directly.
See examples.
Fields with list-update assignment mechanics include:
\code{args}, \code{options}, \code{params}, and \code{paths}, but notably \strong{not} \code{modules}.
}
\examples{
\dontrun{
  ## let's assume the config's arg list contains 3 values:
  ## config$args$arg1 == value1
  ## config$args$arg2 == value2
  ## config$args$arg3 == value3

  ## update an arg element by assigning a (partial) list
  config$args <- list(arg1 = newValue1, arg2 = newValue2)

  ## now args looks like this:
  ## config$args$arg1 == newValue1
  ## config$args$arg2 == newValue2
  ## config$args$arg3 == value3

  ## the following may work (traditional R assignment)
  ## but might also fail silently
  config$args$arg3 <- newValue3  ## works; arg3 update
  config$args$arg3 <- NULL       ## dosen't work; arg3 not removed

  ## the following will work
  config$args <- list(arg3 = newValue3) ## arg3 updated
  config$args <- list(arg3 = NULL)      ## arg3 removed

  ## note, however, that modules list behaves more traditionally:
  ## let's assume config$modules lists 3 modules: mod1, mod2, mod3
  config$modules <- append(config$modules, list(mod4 = "mod4")) ## adds mod4
  config$modules <- list(mod4 = "mod4") ## **replaces** the previous list!

  ## it's a good idea to `update` and `validate` your project config
  ## whenever you manually update a `projConfig` object:
  config$update()    ## required when config$context has changed
  config$validate()
}

}
\author{
Alex Chubaty and Eliot McIntire
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{context}}{a \code{projContext} object.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{args}}{Named list of additional project arguments.}

\item{\code{modules}}{List of module names, which should correspond to the names in \code{params}.}

\item{\code{options}}{Named list of R and R package options to be set.}

\item{\code{params}}{Named list of named lists specifying simulation parameters.
The names of the outermost list must correspond to modules
(and may also include \code{.global}).}

\item{\code{paths}}{Named list of paths, which should include (at minimum) the
the paths in \code{SpaDES.core::setPaths}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-projConfig-new}{\code{projConfig$new()}}
\item \href{#method-projConfig-update}{\code{projConfig$update()}}
\item \href{#method-projConfig-validate}{\code{projConfig$validate()}}
\item \href{#method-projConfig-clone}{\code{projConfig$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-projConfig-new"></a>}}
\if{latex}{\out{\hypertarget{method-projConfig-new}{}}}
\subsection{Method \code{new()}}{
Create an new \code{projConfig} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{projConfig$new(projectName, projectPath, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{projectName}}{character string of length 1 giving the name of the project.}

\item{\code{projectPath}}{character string giving the path to the project directory.}

\item{\code{...}}{Additional arguments passed to \code{useContext()}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-projConfig-update"></a>}}
\if{latex}{\out{\hypertarget{method-projConfig-update}{}}}
\subsection{Method \code{update()}}{
Update a \code{projConfig} object from its context.
Must be called anytime the context is updated.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{projConfig$update()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-projConfig-validate"></a>}}
\if{latex}{\out{\hypertarget{method-projConfig-validate}{}}}
\subsection{Method \code{validate()}}{
Validate the values of fields in a \code{projConfig} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{projConfig$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-projConfig-clone"></a>}}
\if{latex}{\out{\hypertarget{method-projConfig-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{projConfig$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
