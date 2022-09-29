#' Use a pre-defined project run context
#'
#' @param projectName character string of length 1 giving the name of the project.
#'                    Can be one of "LandWeb",
#'                    else will return a minimal set.
#'
#' @param ... additional context arguments used by individual projects.
#'
#' @return named list
#'
#' @export
#'
#' @examples
#' ## default generic context
#' useContext()
#'
#' ## default LandWeb context
#' useContext("LandWeb", mode = "development", version = 3)
useContext <- function(projectName = NULL, ...) {
  if (is.null(projectName)) projectName <- ""

  dots <- list(...)

  .studyAreaName <- if (is.null(dots$studyAreaName)) {
    "LandWeb" ## TODO: is this too big for default study area?
  } else {
    dots$studyAreaName
  }

  .nodename <- Sys.info()[["nodename"]]
  .user <- Sys.info()[["user"]]

  switch(tolower(projectName),
         landweb = {
           if (dots$version == 2) {
             list(
               mode = dots$mode,
               user = .user,
               machine = .nodename,
               dispersalType = "high", ## "aspen", "default", "none"
               forceResprout = TRUE,
               friMultiple = 1L,
               studyAreaName = .studyAreaName, ## multiple others; see LandWeb_preamble
               succession = TRUE, ## FALSE
               pixelSize = 250,
               rep = 1L,
               ROStype = "log"
             )
           } else if (dots$version == 3) {
             list(
               mode = dots$mode,
               user = .user,
               machine = .nodename,
               dispersalType = "default",
               forceResprout = FALSE,
               friMultiple = 1L,
               studyAreaName = paste0(.studyAreaName, "_v3"), ## multiple others; see LandWeb_preamble
               succession = TRUE,
               pixelSize = 250,
               rep = 1L,
               ROStype = "default"
             )
           }
         },
         ## default
         list(
           mode = dots$mode,
           user = .user,
           machine = .nodename,
           studyAreaName = NA_character_,
           rep = 1L
         )
        )
}
