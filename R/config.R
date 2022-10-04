#' Use a pre-defined project run context
#'
#' @param projectName character string of length 1 giving the name of the project.
#'                    Can be one of "LandWeb",
#'                    else will return a default context.
#'
#' @param projectPath character string giving the path to the project directory.
#'
#' @param ... additional context arguments used by individual projects.
#'
#' @return named list
#'
#' @export
#' @importFrom SpaDES.project findProjectName findProjectPath
#'
#' @examples
#' \dontrun{
#' ## default generic project config
#' config <- useConfig("myProject", "path/to/myMproject")
#'
#' ## default LandWeb project config
#' config <- useConfig("LandWeb", "~/GitHub/LandWeb", mode = "development", version = 3)
#' }
useConfig <- function(projectName = NULL, projectPath = NULL, ...) {
  if (is.null(projectName)) {
    projectName <- findProjectName()
  }

  if (is.null(projectPath)) {
    projectPath <- findProjectPath()
  }

  ## additional project types can be added here
  config <- switch(
    tolower(projectName),
    landweb = landwebConfig$new(projectPath = projectPath, ...),
    projConfig$new(projectName = projectName, projectPath = projectPath, ...) ## default
  )
  config$update()
  config$validate()

  return(config)
}

#' Project configuration class
#'
#' @author Alex Chubaty and Eliot McIntire
# @export projConfig
#' @importFrom R6 R6Class
#' @importFrom Require modifyList2 normPath
#' @rdname projConfig-class
projConfig <- R6::R6Class(
  "projConfig",

  public = list(
    #' @field context a `projContext` object.
    context = NULL,

    #' @description Create an new `projConfig` object
    #'
    #' @param projectName character string of length 1 giving the name of the project.
    #'                    Can be one of "LandWeb",
    #'                    else will return a default context.
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments passed to `useContext()`
    #'
    initialize = function(projectName, projectPath, ...) {
      self$context <- useContext(projectName, projectPath, ...)

      ## do paths first as these may be used below
      private$.paths = list(
        cachePath = "cache",
        inputPath = "inputs",
        modulePath = "modules",
        outputPath = "outputs",
        projectPath = normPath(projectPath)
      )

      private$.args = list()
      private$.modules = list()
      private$.options = list()
      private$.params = list(.globals = list())

      invisible(self)
    },

    #' @description Update a `LandWebConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      return(invisible(self))
    },

    #' @description Validate the values of fields in a `projConfig` object
    #'
    #' @importFrom Require modifyList2
    validate = function() {
      ## check all modules exist in project --------------------------------------------------------
      fullModulePath <- normPath(file.path(self$paths$projectPath, self$paths$modulePath))
      modsInPrj <- list.dirs(fullModulePath, recursive = FALSE, full.names = FALSE)
      if (!all(self$modules %in% modsInPrj)) {
        warning("modules list contains modules not found in modulePath ", self$paths$modulePath)
      }

      ## check user-specified params against module metadata ---------------------------------------
      ## TODO: what to do with defaults using e.g., `start(sim)`
      if (requireNamespace("SpaDES.core", quietly = TRUE)) {
        params_ <- lapply(self$modules, function(m) {
          pdt <- SpaDES.core::moduleParams(m, fullModulePath)
          p <- structure(.Data = pdt$default, names = pdt$paramName) ## named list
        })
        names(params_) <- self$modules

        params_ <- lapply(names(params_), function(x) {
          modifyList2(params_[[x]], self$params[[x]]) ## TODO: check that dot params are being seen/read
        })
        names(params_) <- self$modules

        self$params <- params_
      }

      ## check that known options using paths are correct ------------------------------------------
      stopifnot(
        normPath(self$options$map.dataPath) == normPath(self$paths$inputPath),
        normPath(self$options$map.tilePath) == normPath(self$paths$tilePath),
        normPath(self$options$reproducible.destinationPath) == normPath(self$paths$inputPath)
      )

      invisible(self)
    }
  ),

  active = list(
    #' @field args   Named list of additional project arguments.
    args = function(value) {
      if (missing(value)) {
        return(private$.args)
      } else {
        private$.args <- modifyList2(private$.args, as.list(value))
      }
    },

    #' @field modules List of module names, which should correspond to the names in `params`.
    modules = function(value) {
      if (missing(value)) {
        return(private$.modules)
      } else {
        ## allow passing partial list to exclude modules, instead of simply using:
        ## private$.modules <- modifyList2(self$modules, modules)

        updatedModules <- as.list(value) ## ensure it's a list

        if (is.null(names(updatedModules))) {
          names(updatedModules) <- updatedModules ## ensure it's a named list
        }

        private$.modules <- updatedModules
      }
    },

    #' @field options Named list of R and R package options to be set.
    options = function(value) {
      if (missing(value)) {
        return(private$.options)
      } else {
        private$.options <- modifyList2(private$.options, as.list(value))
      }
    },

    #' @field params  Named list of named lists specifying simulation parameters.
    #'               The names of the outermost list must correspond to modules
    #'               (and may also include `.global`).
    params = function(value) {
      if (missing(value)) {
        return(private$.params)
      } else {
        ## check for params being passed to modules not listed in self$modules
        moduleNames <- names(self$modules)
        passedParamNames <- names(value)
        unknownModules <- passedParamNames[which(!passedParamNames %in% moduleNames)]
        unknownModules <- unknownModules[!unknownModules %in% c(".globals")]
        if (length(unknownModules) > 0) {
          warning("Parameters specified for modules not found in `modules` and will be ignored:\n",
                  paste(unknownModules, collapse = "\n"))
        }

        mods2keep <- c(".globals", moduleNames)
        params_ <- subset(private$.params, names(private$.params) %in% mods2keep)
        params_ <- lapply(mods2keep, function(x) {
          modifyList2(params_[[x]], value[[x]])
        })
        names(params_) <- mods2keep

        ## TODO: if user updates global params, propagate this change to corresponding module params.
        ##       should user be warned if trying to update module pram that would be overridden by global?

        private$.params <- params_
      }
    },

    #' @field paths  Named list of paths, which should include (at minimum) the
    #'              the paths in `SpaDES.core::setPaths`.
    paths = function(value) {
      if (missing(value)) {
        return(private$.paths)
      } else {
        ## update paths
        updatedPaths <- modifyList2(private$.paths, value)

        ## ensure paths are kept relative to projectPath except for scratch
        pathNames <- names(updatedPaths)
        updatedPaths <- lapply(pathNames, function(pthnm) {
          if (pthnm %in% c("projectPath", "scratchPath")) {
            updatedPaths[[pthnm]]
          } else {
            .updateRelativePath(updatedPaths[[pthnm]], private$.paths$projectPath)
          }
        })
        names(updatedPaths) <- pathNames

        updatedPaths$tilePath <- file.path(updatedPaths$outputPath, "tiles")

        private$.paths <- updatedPaths

        ## update known paths in options
        if ("map.dataPath" %in% names(self$options)) {
          private$.options$map.dataPath <- private$.paths$inputPath
          attr(private$.options$map.dataPath, "auto") <- TRUE
        }

        if ("map.tilePath" %in% names(self$options)) {
          private$.options$map.tilePath <- private$.paths$tilePath
          attr(private$.options$map.tilePath, "auto") <- TRUE
        }

        if ("reproducible.destinationPath" %in% names(self$options)) {
          private$.options$reproducible.destinationPath <- private$.paths$inputPath
          attr(private$.options$reproducible.destinationPath, "auto") <- TRUE
        }
      }
    }
  ),

  private = list(
    .args = list(),
    .modules = list(),
    .options = list(),
    .params = list(.globals = list()),
    .paths = list()
  )
)
