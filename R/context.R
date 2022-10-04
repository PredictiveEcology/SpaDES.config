#' Use a pre-defined project run context
#'
#' @param projectName character string of length 1 giving the name of the project.
#'                    Can be one of "LandWeb",
#'                    else will return a default context.
#'
#' @param projectPath Character string giving the path to the project directory.
#'
#' @param ... additional context arguments used by individual projects.
#'
#' @return a `projContext` object.
#'
#' @export
#' @importFrom SpaDES.project findProjectName
#'
#' @examples
#' \dontrun{
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
    landweb = landwebContext$new(projectPath = projectPath, ...),
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
    #'             or 'profile.
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param studyAreaName Character string identifying a study area (see `LandWeb_preamble`
    #'                      module for up-to-date descriptions of each study area label).
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
