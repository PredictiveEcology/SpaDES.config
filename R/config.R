#' Use a pre-defined project run context
#'
#' @param projectName character string of length 1 giving the name of the project.
#'                    Can be one of "LandWeb", "BC_NRV", "LandRfS",
#'                    else will return a default context.
#'
#' @param projectPath Character string giving the path to the project directory.
#'
#' @param ... additional context arguments used by individual projects.
#'
#' @return a `projContext` object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' ## default generic context
#' useContext("myProject", "path/to/myProject")
#'
#' ## default LandWeb context
#' useContext("LandWeb", "~/GitHub/LandWeb",
#'            mode = "development", rep = 1L, studyAreaName = "LandWeb", version = 3)
#' }
useContext <- function(projectName = NULL, projectPath = NULL, ...) {
  if (is.null(projectName)) {
    projectName <- findProjectName()
  }

  if (is.null(projectPath)) {
    projectPath <- findProjectPath()
  }

  switch(
    tolower(projectName),
    bc_nrv = bcnrvContext$new(projectPath = projectPath, ...),
    landweb = landwebContext$new(projectPath = projectPath, ...),
    landrfs = landrfsContext$new(projectPath = projectPath, ...),
    projContext$new(projectPath = projectPath, ...) ## default
  )
}

#' Project context class
#'
#' @author Alex Chubaty
# @export
#' @importFrom R6 R6Class
#' @rdname projContext-class
projContext <- R6::R6Class(
  "projContext",

  public = list(
    #' @field machine  Character string giving the name of the current machine (i.e., nodename)
    machine = character(1),

    #' @field user  Character string giving the username on the current machine
    user = character(1),

    ## methods -------------------------------------------------------------------------------------
    #' @description Create a new `projContext` object
    #'
    #' @param projectPath Character string giving the path to the project directory.
    #'
    #' @param mode Character string. One of 'production', 'development', 'postprocess',
    #'             or 'profile'.
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param studyAreaName Character string identifying a study area.
    initialize = function(projectPath, mode = "development", rep = 1L, studyAreaName = "default") {
      private[[".mode"]] <- NA_character_
      private[[".projectPath"]] <- normPath(projectPath)
      private[[".studyAreaName"]] <- NA_character_
      private[[".rep"]] <- NA_integer_
      private[[".runName"]] <- NA_character_
      attr(private[[".runName"]], "auto") <- TRUE

      self$machine <- Sys.info()[["nodename"]]
      self$user <- Sys.info()[["user"]]

      self$mode <- mode
      self$rep <- rep
      self$studyAreaName <- studyAreaName

      return(invisible(self))
    },

    #' @description print the context object in markdown table format,
    #'              and invisibly return this formatted table for use
    #'              e.g., when writing the context info to a file for humans.
    print = function() {
      cntxt <- list(
        mode = private[[".mode"]],
        machine = self$machine,
        user = self$user,
        studyAreaName = private[[".studyAreaName"]],
        rep = private[[".rep"]],
        runName = private[[".runName"]]
      )

      info <- .context2md(cntxt)

      message(info)

      return(invisible(info))
    }
  ),

  active = list(
    #' @field mode  Character string giving the project run mode.
    #'              E.g., can be used to differentiate among "production".
    #'              "development", or "postprocessing" runs.
    mode = function(value) {
      if (missing(value)) {
        return(private[[".mode"]])
      } else {
        private[[".mode"]] <- tolower(value)
      }
    },

    #' @field studyAreaName  Character string giving the name of current study area.
    studyAreaName = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaName"]])
      } else {
        ## TODO: issues getting relative paths when studyAreaName == projDir
        ## workaround is to append some suffix to the studyAreaName
        private[[".studyAreaName"]] <- if (identical(value, basename(private[[".projectPath"]]))) {
          paste0(value, "_full")
        } else {
          value
        }
        private[[".runName"]] <- sprintf("%s_rep%02d", private[[".studyAreaName"]], private[[".rep"]])
      }
    },

    #' @field rep  replicate id (integer)
    rep = function(value) {
      if (missing(value)) {
        return(private[[".rep"]])
      } else {
        private[[".rep"]] <- as.integer(value)
        private[[".runName"]] <- sprintf("%s_rep%02d", private[[".studyAreaName"]], private[[".rep"]])
      }
    },

    #' @field runName  get or set the current runName
    runName = function(value) {
      if (missing(value)) {
        return(private[[".runName"]])
      } else {
        private[[".runName"]] <- value
      }
    }
  ),
  private = list(
    .mode = NA_character_,
    .projectPath = NA_character_,
    .rep = NA_integer_,
    .runName  = NA_character_,
    .studyAreaName = NA_character_
  )
)

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
#' @return a `projConfig` object
#'
#' @export
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
    bc_nrv = bcnrvConfig$new(projectName = projectName, projectPath = projectPath, ...),
    landweb = landwebConfig$new(projectName = projectName, projectPath = projectPath, ...),
    landrfs = landrfsConfig$new(projectName = projectName, projectPath = projectPath, ...),
    projConfig$new(projectName = projectName, projectPath = projectPath, ...) ## default
  )$update()$validate()

  return(config)
}

#' Project configuration class
#'
#' @note Several fields use a update their list values when assigning to them,
#' rather than replacing the entire list as might be expected with traditional R assignment.
#' This can make updating lists easier (don't need to worry about copy existing values
#' or otherwise worry about appending/updating the list; or accidentally dropping elements),
#' and allows for checking or validation of values upon assignment.
#' Thus, the user should assign a list containing the (named) sub-element to be updated rather
#' than attempting to assign to a sub-element directly.
#' See examples.
#' Fields with list-update assignment mechanics include:
#'   `args`, `options`, `params`, and `paths`, but notably **not** `modules`.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export projConfig
#' @importFrom R6 R6Class
#' @importFrom Require modifyList3 normPath
#'
#' @examples
#' \dontrun{
#'   ## let's assume the config's arg list contains 3 values:
#'   ## config$args$arg1 == value1
#'   ## config$args$arg2 == value2
#'   ## config$args$arg3 == value3
#'
#'   ## update an arg element by assigning a (partial) list
#'   config$args <- list(arg1 = newValue1, arg2 = newValue2)
#'
#'   ## now args looks like this:
#'   ## config$args$arg1 == newValue1
#'   ## config$args$arg2 == newValue2
#'   ## config$args$arg3 == value3
#'
#'   ## the following may work (traditional R assignment)
#'   ## but might also fail silently
#'   config$args$arg3 <- newValue3  ## works; arg3 update
#'   config$args$arg3 <- NULL       ## dosen't work; arg3 not removed
#'
#'   ## the following will work
#'   config$args <- list(arg3 = newValue3) ## arg3 updated
#'   config$args <- list(arg3 = NULL)      ## arg3 removed
#'
#'   ## note, however, that modules list behaves more traditionally:
#'   ## let's assume config$modules lists 3 modules: mod1, mod2, mod3
#'   config$modules <- append(config$modules, list(mod4 = "mod4")) ## adds mod4
#'   config$modules <- list(mod4 = "mod4") ## **replaces** the previous list!
#'
#'   ## it's a good idea to `update` and `validate` your project config
#'   ## whenever you manually update a `projConfig` object:
#'   config$update()    ## required when config$context has changed
#'   config$validate()
#' }
#'
#' @rdname projConfig-class
projConfig <- R6::R6Class(
  "projConfig",

  public = list(
    #' @field context a `projContext` object.
    context = NULL,

    #' @description Create an new `projConfig` object
    #'
    #' @param projectName character string of length 1 giving the name of the project.
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments passed to `useContext()`
    #'
    initialize = function(projectName, projectPath, ...) {
      self$context <- useContext(projectName, projectPath, ...)

      ## do paths first as these may be used below
      private[[".paths"]] <- list(
        cachePath = .baseCachePath,
        inputPath = .baseInputPath,
        inputPaths = .baseDataCachePath,
        logPath = .baseLogPath,
        modulePath = .baseModulePath,
        outputPath = .baseOutputPath,
        projectPath = normPath(projectPath)
      )

      private[[".args"]] <- list()
      private[[".modules"]] <- list()
      private[[".options"]] <- list()

      ## need to keep copy of all default params for when modules updated
      private[[".params_full"]] <- list(.globals = list())

      self$params <- private[[".params_full"]]

      invisible(self)
    },

    #' @description Update a `projConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      return(invisible(self))
    },

    #' @description Validate the values of fields in a `projConfig` object
    #'
    #' @importFrom Require modifyList3
    validate = function() {
      ## check all modules exist in project --------------------------------------------------------
      fullModulePath <- normPath(file.path(self$paths[["projectPath"]], self$paths[["modulePath"]]))

      modsInPrj <- list.dirs(fullModulePath, recursive = FALSE, full.names = FALSE)
      if (!all(self$modules %in% modsInPrj)) {
        warning("modules list contains modules not found in modulePath ", self$paths[["modulePath"]])
      }

      ## check user-specified params against module metadata ---------------------------------------
      if (requireNamespace("SpaDES.core", quietly = TRUE)) {
        params_ <- lapply(self$modules, function(m) {
          message("Validating parameters for module ", m, " ...")
          pdt <- SpaDES.core::moduleParams(m, fullModulePath)
          p <- structure(.Data = pdt$default, names = pdt$paramName) ## named list
        })
        names(params_) <- self$modules

        params_ <- lapply(names(params_), function(x) {
          if (length(self$params[[x]]) == 0) {
            ## missing parameters likely means the module was not originally in the list
            ## pull in the param values from the full list
            modifyList3(params_[[x]], private[[".params_full"]][[x]])
          } else {
            modifyList3(params_[[x]], self$params[[x]])
          }
        })
        names(params_) <- self$modules

        ## TODO: deal with defaults using e.g., `start(sim)`
        ## but for now, warn the user to supply actual values
        params_ <- lapply(names(params_), function(x) {
          if (any(grepl("sim", params_[[x]]))) {
            ids <- which(grepl("\\(sim\\)", params_[[x]]))
            message("NOTE: parameters in module ", x, " contain `sim`: ",
                    paste(names(params_[[x]][ids]), collapse = ", "), ".")
          }
          params_[[x]]
        })
        names(params_) <- self$modules

        self$params <- params_
      }

      ## check that known options using paths are correct ------------------------------------------
      stopifnot(
        normPath(self$options[["map.dataPath"]]) == normPath(self$paths[["inputPath"]]),
        normPath(self$options[["map.tilePath"]]) == normPath(self$paths[["tilePath"]]),
        normPath(self$options[["reproducible.destinationPath"]]) == normPath(self$paths[["inputPath"]])
      )

      invisible(self)
    }
  ),

  active = list(
    #' @field args   Named list of additional project arguments.
    args = function(value) {
      if (missing(value)) {
        return(private[[".args"]])
      } else {
        private[[".args"]] <- modifyList3(private[[".args"]], as.list(value))
      }
    },

    #' @field modules List of module names, which should correspond to the names in `params`.
    modules = function(value) {
      if (missing(value)) {
        return(private[[".modules"]])
      } else {
        ## allow passing partial list to exclude modules, instead of simply using:
        ## private[[".modules"]] <- modifyList3(self$modules, modules)

        updatedModules <- as.list(value) ## ensure it's a list

        if (is.null(names(updatedModules))) {
          names(updatedModules) <- updatedModules ## ensure it's a named list
        }

        private[[".modules"]] <- updatedModules
      }
    },

    #' @field options Named list of R and R package options to be set.
    options = function(value) {
      if (missing(value)) {
        return(private[[".options"]])
      } else {
        private[[".options"]] <- modifyList3(private[[".options"]], as.list(value))
      }
    },

    #' @field params  Named list of named lists specifying simulation parameters.
    #'               The names of the outermost list must correspond to modules
    #'               (and may also include `.global`).
    params = function(value) {
      if (missing(value)) {
        return(private[[".params"]])
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
        params_ <- subset(private[[".params"]], names(private[[".params"]]) %in% mods2keep)
        params_ <- lapply(mods2keep, function(x) {
          tmp <- modifyList3(params_[[x]], value[[x]])
          if (x != ".globals") {
            ## if user updates global params, propagate this change to corresponding module params
            globals_ <- subset(params_[[".globals"]], names(params_[[".globals"]]) %in% names(tmp))
            tmp <- modifyList3(tmp, globals_)
          }

          tmp
        })
        names(params_) <- mods2keep

        ## keep track of parameter changes in the complete list
        private[[".params_full"]] <- modifyList3(private[[".params_full"]], params_)

        ## set current params to only be the subset of those in config$modules
        private[[".params"]] <- params_
      }
    },

    #' @field paths  Named list of paths, which should include (at minimum) the
    #'              the paths in `SpaDES.core::setPaths`.
    paths = function(value) {
      if (missing(value)) {
        return(private[[".paths"]])
      } else {
        ## update paths
        updatedPaths <- modifyList3(private[[".paths"]], value)

        ## ensure paths are kept relative to projectPath except for scratch dirs
        pathNames <- names(updatedPaths)
        updatedPaths <- lapply(pathNames, function(pthnm) {
          if (pthnm %in% c("projectPath", "scratchPath")) {
            updatedPaths[[pthnm]]
          } else {
            .updateRelativePath(updatedPaths[[pthnm]], private[[".paths"]][["projectPath"]])
          }
        })
        names(updatedPaths) <- pathNames

        updatedPaths[["logPath"]] <- file.path(updatedPaths[["outputPath"]], "log")
        updatedPaths[["tilePath"]] <- file.path(updatedPaths[["outputPath"]], "tiles")

        private[[".paths"]] <- updatedPaths

        ## update known paths in options
        if ("map.dataPath" %in% names(self$options)) {
          private[[".options"]][["map.dataPath"]] <- private[[".paths"]][["inputPath"]]
          attr(private[[".options"]][["map.dataPath"]], "auto") <- TRUE
        }

        if ("map.tilePath" %in% names(self$options)) {
          private[[".options"]][["map.tilePath"]] <- private[[".paths"]][["tilePath"]]
          attr(private[[".options"]][["map.tilePath"]], "auto") <- TRUE
        }

        if ("reproducible.destinationPath" %in% names(self$options)) {
          private[[".options"]][["reproducible.destinationPath"]] <- private[[".paths"]][["inputPath"]]
          attr(private[[".options"]][["reproducible.destinationPath"]], "auto") <- TRUE
        }
      }
    }
  ),

  private = list(
    .args = list(),
    .modules = list(),
    .options = list(),
    .params = list(.globals = list()),
    .params_full = list(.globals = list()),
    .paths = list()
  )
)
