#' Project configuration class
#'
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export projConfig
#' @importFrom R6 R6Class
#' @rdname projConfig-class
projConfig <- R6::R6Class(
  "projConfig",

  public = list(
    #' @field args   Named list of additional project arguments.
    args = list(),

    #' @field modules List of module names, which should correspond to the names in `params`.
    modules = list(),

    #' @field options Named list of R and R package options to be set.
    options = list(),

    #' @field params  Named list of named lists specifying simulation parameters.
    #'               The names of the outermost list must correspond to modules
    #'               (and may also include `.global`).
    params = list(),

    #' @field paths  Named list of paths, which should include (at minimum) the
    #'              the paths in `SpaDES.core::setPaths`.
    paths = list(),

    #' @description Create an new `projConfig` object
    initialize = function() {
      self$args = list()
      self$modules = list()
      self$options = list()
      self$params = list()
      self$paths = list(
        cachePath = "cache",
        inputPath = "inputs",
        modulePath = "modules",
        outputPath = "outputs",
        projectPath = normPath(".")
      )

      invisible(self)
    },

    #' @description Update the fields in a `projConfig` object
    #'
    #' The objects passed as arguments should match the fields of the `projConfig` object.
    #'
    #' @param args named list; see [projConfig].
    #' @param modules named list; see [projConfig].
    #' @param methods named list; see [projConfig].
    #' @param options named list; see [projConfig].
    #' @param params named list; see [projConfig].
    #' @param paths named list; see [projConfig].
    #'
    #' @importFrom Require modifyList2 normPath
    update = function(args = NULL, modules = NULL, options = NULL, params = NULL, paths = NULL) {
      ## need to update paths first, as these are potentially used by fields
      self$paths <- modifyList2(self$paths, paths)

      pathNames <- names(self$paths)
      updatedPaths <- lapply(pathNames, function(pthnm) {
        if (pthnm %in% c("projectPath", "scratchPath")) {
          self$paths[[pthnm]]
        } else {
          .updateRelativePath(self$paths[[pthnm]], self$paths$projectPath)
        }
      })
      names(updatedPaths) <- pathNames

      updatedPaths$tilePath <- file.path(updatedPaths$outputPath, "tiles")

      updatedPaths <- lapply(updatedPaths, function(pth) {
        if (!is.null(pth)) normPath(pth) else NULL ## don't create paths here
      })

      self$paths <- updatedPaths

      if (!is.null(modules)) {
        if (is.null(names(modules))) names(modules) <- modules
      }

      self$args <- modifyList2(self$args, args)

      self$modules <- modifyList2(self$modules, modules)

      self$options <- modifyList2(self$options, options)

      ## update known paths in options
      if ("map.dataPath" %in% names(self$options)) {
        self$options$map.dataPath <- self$paths$inputPath
      }

      if ("map.tilePath" %in% names(self$options)) {
        self$options$map.tilePath <- self$paths$tilePath
      }

      if ("reproducible.destinationPath" %in% names(self$options)) {
        self$options$reproducible.destinationPath <- self$paths$inputPath
      }

      if (is.null(params)) {
        params <- list()
      } else {
        ## check for params being passed to modules not listed in self$modules
        unknownModules <- names(params)[which(!names(params) %in% names(self$modules))]
        unknownModules <- unknownModules[!unknownModules %in% c(".globals")]
        if (length(unknownModules) > 0) {
          warning("Parameters specified for modules not found in `modules` and will be ignored:\n",
                  paste(unknownModules, collapse = "\n"))
        }
      }

      params_ <- self$params
      params_ <- lapply(names(params_), function(x) {
        modifyList2(params_[[x]], params[[x]])
      })
      names(params_) <- names(self$params)

      self$params <- params_

      invisible(self)
    },

    #' @description Validate the values of fields in a `projConfig` object
    #'
    #' @importFrom Require modifyList2
    validate = function() {
      ## check all modules exist in project --------------------------------------------------------
      modsInPrj <- list.dirs(self$paths$modulePath, recursive = FALSE, full.names = FALSE)
      if (!all(self$modules %in% modsInPrj)) {
        warning("modules list contains modules not found in modulePath ", self$paths$modulePath)
      }

      ## check user-specified params against module metadata ---------------------------------------
      ## TODO: what to do with defaults using e.g., `start(sim)`
      if (requireNamespace("SpaDES.core", quietly = TRUE)) {
        params_ <- lapply(self$modules, function(m) {
          pdt <- SpaDES.core::moduleParams(m, self$paths$modulePath)
          p <- structure(.Data = pdt$default, names = pdt$paramName) ## named list
        })
        names(params_) <- self$modules

        params_ <- lapply(names(params_), function(x) {
          modifyList2(params_[[x]], self$params[[x]]) ## TODO: ensure user hasn't passed unused params
        })
        names(params_) <- self$modules
      }

      ## check that known options using paths are correct ------------------------------------------
      stopifnot(
        identical(normPath(self$options$map.dataPath), normPath(self$paths$inputPath)),
        identical(normPath(self$options$map.tilePath), normPath(self$paths$tilePath)),
        identical(normPath(self$options$reproducible.destinationPath), normPath(self$paths$inputPath))
      )

      invisible(self)
    }
  )
)
