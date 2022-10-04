#' @keywords internal
.landwebRunName <- function(context, withRep = TRUE) {
  .runName <- paste0(
    context$studyAreaName,
    if (context$dispersalType == "default") "" else paste0("_", context$dispersalType, "Dispersal"),
    if (context$ROStype == "default") "" else paste0("_", context$ROStype, "ROS"),
    if (isTRUE(context$succession)) "" else "_noSuccession",
    if (context$friMultiple == 1) "" else paste0("_fri", context$friMultiple),
    if (context$pixelSize == 250) "" else paste0("_res", context$pixelSize),
    if (isTRUE(withRep)) {
      if (context$mode == "postprocess") "" else sprintf("_rep%02d", context$rep)
    } else {
      ""
    }
  )
  attr(.runName, "auto") <- TRUE

  return(.runName)
}

#' LandWeb project context class
#'
#' This extends the `projContext` class by setting various LandWeb defaults and
#' employing custom fied validation.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname landwebContext-class
landwebContext <- R6::R6Class(
  "landwebContext",
  inherit = projContext,

  public = list(
    #' @param projectPath Character string giving the path to the project directory.
    #'
    #' @param mode Character string. One of 'production', 'development', 'postprocess',
    #'             or 'profile.
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param studyAreaName Character string identifying a study area (see `LandWeb_preamble`
    #'                      module for up-to-date descriptions of each study area label).
    #'
    #' @param version Integer. Shorthand denoting whether vegetation parameter forcings (`version = 2`)
    #'                should be used as they were for the ca. 2018 runs.
    #'                Version 3 uses the default LandR Biomass parameters (i.e., no forcings).
    initialize = function(projectPath, mode = "development", rep = 1L, studyAreaName = "random", version = 3) {
      stopifnot(version %in% c(2, 3))

      private[[".dispersalType"]] <- if (version == 2) "high" else if (version == 3) "default"
      private[[".forceResprout"]] <- if (version == 2) TRUE else if (version == 3) FALSE
      private[[".friMultiple"]] <- 1L
      private[[".pixelSize"]] <- 250
      private[[".projectPath"]] <- normPath(projectPath)
      private[[".ROStype"]] <- if (version == 2) "log" else if (version == 3) "default"
      private[[".succession"]] <- TRUE
      private[[".version"]] <- as.integer(version)

      self$machine <- Sys.info()[["nodename"]]
      self$user <- Sys.info()[["user"]]

      self$mode <- mode
      self$rep <- rep
      self$studyAreaName <- studyAreaName

      self$runName <- .landwebRunName(self)

      return(invisible(self))
    },

    #' @description print the context object in markdown table format,
    #'              and invisibly return this formatted table for use
    #'              e.g., when writing the context info to a file for humans.
    print = function() {
      cntxt <- list(
        mode = self$mode,
        machine = self$machine,
        user = self$user,
        studyAreaName = self$studyAreaName,
        rep = self$rep,
        dispersalType = self$dispersalType, ## additional for landweb
        forceResprout = self$forceResprout, ## additional for landweb
        friMultiple = self$friMultiple,     ## additional for landweb
        pixelSize = self$pixelSize,         ## additional for landweb
        ROStype = self$ROStype,             ## additional for landweb
        succession = self$succession,       ## additional for landweb
        runName = self$runName
      )

      info <- .context2md(cntxt)

      message(info)

      return(invisible(info))
    }
  ),

  active = list(
    #' @field mode  Character string giving the project run mode.
    #'              One of 'production', 'development', 'postprocess', or 'profile'.
    mode = function(value) {
      if (missing(value)) {
        return(private[[".mode"]])
      } else {
        stopifnot(tolower(value) %in% c("production", "development", "postprocess", "profile"))
        private[[".mode"]] <- tolower(value)

        if (private[[".mode"]] == "postprocess") {
          self$rep <- NA_integer_
        }
      }
    },

    #' @field studyAreaName  Character string giving the name of current study area.
    studyAreaName = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaName"]])
      } else {
        ## TODO: issues getting relative paths when studyAreaName == projDir
        ## workaround is to append some suffix to the studyAreaName (e.g., LandWeb_full)
        newValue <- if (identical(value, basename(private[[".projectPath"]]))) {
          paste0(value, "_full")
        } else {
          value
        }

        newValue <- if (private[[".version"]] == 2) {
          newValue
        } else {
          paste0(newValue, "_v", private[[".version"]])
        }

        private[[".studyAreaName"]] <- newValue
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field rep  replicate id (integer)
    rep = function(value) {
      if (missing(value)) {
        return(private[[".rep"]])
      } else {
        if (private[[".mode"]] == "postprocess" && !is.na(value)) {
          warning("unable to set context$rep because context$mode == 'postprocess'")
        } else {
          private[[".rep"]] <- as.integer(value)
          self$runName <- .landwebRunName(self)
        }
      }
    },

    #' @field dispersalType Character string describing the seed dispersal type to use.
    #'                      One of 'default', 'aspen', 'high', 'none'.
    dispersalType = function(value) {
      if (missing(value)) {
        return(private[[".dispersalType"]])
      } else {
        stopifnot(value %in% c("default", "aspen", "high", "none"))
        private[[".dispersalType"]] <- value
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field forceResprout Logical
    forceResprout = function(value) {
      if (missing(value)) {
        return(private[[".forceResprout"]])
      } else {
        private[[".forceResprout"]] <- value
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field friMultiple Numeric indicating a factor by which to scale the fire return intervals
    friMultiple = function(value) {
      if (missing(value)) {
        return(private[[".friMultiple"]])
      } else {
        private[[".friMultiple"]] <- value
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field pixelSize raster pixel resolution (in metres) to use for simulations
    pixelSize = function(value) {
      if (missing(value)) {
        return(private[[".pixelSize"]])
      } else {
        stopifnot(value %in% c(250, 125, 50))
        private[[".pixelSize"]] <- value
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field ROStype  Character string describing the scaling of the LandMine fire model's
    #'                 'rate of spread' parameters.
    #'                 One of 'default' (i.e., none), 'equal' (i.e., all 1), 'log'.
    ROStype = function(value) {
      if (missing(value)) {
        return(private[[".ROStype"]])
      } else {
        stopifnot(value %in% c("default", "equal", "log"))
        private[[".ROStype"]] <- value
        self$runName <- .landwebRunName(self)
      }
    },

    #' @field succession  logical
    succession = function(value) {
      if (missing(value)) {
        return(private[[".succession"]])
      } else {
        private[[".succession"]] <- value
        self$runName <- .landwebRunName(self)
      }
    }
  ),

  private = list(
    .dispersalType = NA_character_,
    .forceResprout = NA,
    .friMultiple = 1,
    .pixelSize = 250,
    .ROStype = NA_character_,
    .succession = NA,
    .version = NA_integer_
  )
)
