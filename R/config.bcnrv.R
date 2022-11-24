#' @keywords internal
.bcnrvRunName <- function(context, withRep = TRUE) {
  .runName <- paste0(
    context$studyAreaName,
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

#' BC NRV project context class
#'
#' This extends the `projContext` class by setting various defaults for BC NRV
#' and employing custom field validation.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname bcnrvContext-class
bcnrvContext <- R6::R6Class(
  "bcnrvContext",
  inherit = projContext,

  public = list(
    #' @param projectPath Character string giving the path to the project directory.
    #'
    #' @param mode Character string. One of 'production', 'development', or 'postprocess'.
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param res Numeric indicating the map resolution (pixel size) to use.
    #'            Must be one of 50, 125 (default), 250.
    #'
    #' @param studyAreaName Character string identifying a study area (see `BC_HRV_preamble`
    #'                      module for up-to-date descriptions of each study area label).
    #'
    #' @param version Integer. Shorthand denoting whether vegetation parameter forcings (`version = 2`)
    #'                should be used as they were for the ca. 2018 runs.
    #'                Version 3 uses the default LandR Biomass parameters (i.e., no forcings).
    initialize = function(projectPath, mode = "development", rep = 1L, res = 125, studyAreaName = "Chine") {
      stopifnot(
        res %in% c(50, 125, 250)
      )

      private[[".pixelSize"]] <- res
      private[[".projectPath"]] <- normPath(projectPath)

      self$machine <- Sys.info()[["nodename"]]
      self$user <- Sys.info()[["user"]]

      self$mode <- mode
      self$rep <- rep
      self$studyAreaName <- studyAreaName ## will set studyAreaHash

      self$runName <- .bcnrvRunName(self)

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
        pixelSize = self$pixelSize,
        runName = self$runName
      )

      info <- .context2md(cntxt)

      message(info)

      return(invisible(info))
    }
  ),

  active = list(
    #' @field mode  Character string giving the project run mode.
    #'              One of 'production', 'development', or 'postprocess'.
    mode = function(value) {
      if (missing(value)) {
        return(private[[".mode"]])
      } else {
        stopifnot(tolower(value) %in% c("production", "development", "postprocess"))
        private[[".mode"]] <- tolower(value)

        if (private[[".mode"]] == "postprocess") {
          self$rep <- NA_integer_
        }
      }
    },

    #' @field studyAreaHash  Character string giving the hash of current study area (read-only).
    studyAreaHash = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaHash"]])
      } else {
        stop("studyAreaHash cannot be set directly. ",
             "It is automatically updated with studyAreaName.")
      }
    },

    #' @field studyAreaName  Character string giving the name of current study area.
    studyAreaName = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaName"]])
      } else {
        private[[".studyAreaHash"]] <- if (requireNamespace("reproducible", quietly = TRUE)) {
          reproducible::studyAreaName(value)
        } else {
          .needPkg("reproducible", "stop")
        }
        private[[".studyAreaName"]] <- sprintf("multiple_LUs_n%02d_%s", length(value), self$studyAreaHash)
        self$runName <- .bcnrvRunName(self)
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
          self$runName <- .bcnrvRunName(self)
        }
      }
    },

    #' @field pixelSize raster pixel resolution (in metres) to use for simulations
    pixelSize = function(value) {
      if (missing(value)) {
        return(private[[".pixelSize"]])
      } else {
        stopifnot(value %in% c(250, 125, 50))
        private[[".pixelSize"]] <- value
        self$runName <- .bcnrvRunName(self)
      }
    }
  ),

  private = list(
    .pixelSize = 125,
    .studyAreaHash = NA_character_
  )
)

#' BC NRV project configuration class
#'
#' This extends the `projConfig` class by setting various BC NRV config defaults,
#' and implements custom validation and finalizer methods.
#'
#' @note See note in `?projConfig` describing the list-update mechanism of assignment to
#' certain fields.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname bcnrvConfig-class
bcnrvConfig <- R6::R6Class(
  "bcnrvConfig",
  inherit = projConfig,
  public = list(
    #' @description Create an new `bcnrvConfig` object
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments passed to `useContext()`
    #'
    initialize = function(projectPath, ...) {
      dots <- list(...)

      self$context <- useContext(projectName = "BC_NRV", projectPath = projectPath, ...)

      ## do paths first as these may be used below
      # paths ---------------------------------------------------------------------------------------
      private[[".paths"]] <- list(
        cachePath = .baseCachePath,
        inputPath = .baseInputPath,
        inputPaths = .baseDataCachePath,
        logPath = .baseLogPath,
        modulePath = c(.baseModulePath, file.path(.baseModulePath, "scfm", .baseModulePath)),
        outputPath = .baseOutputPath,
        projectPath = normPath(projectPath),
        scratchPath = file.path(dirname(tempdir()), "scratch", "BC_HRV"),
        tilePath = file.path(.baseOutputPath, "tiles")
      )

      # arguments -----------------------------------------------------------------------------------
      private[[".args"]] <- list(
        cloud = list(
          cacheDir = "BC_HRV_cloudCache",
          googleUser = "",
          useCloud = FALSE
        ),
        delayStart = 0,
        endTime = 1000,
        notifications = list(
          slackChannel = ""
        ),
        useCache = FALSE ## simulation caching
      )

      # modules ------------------------------------------------------------------------------------
      private[[".modules"]] <- list(
        # ageModule = "ageModule", ## with scfm ## removed 2022-11; not needed with LandR
        BC_HRV_preamble = "BC_HRV_preamble",
        Biomass_borealDataPrep = "Biomass_borealDataPrep",
        Biomass_core = "Biomass_core",
        Biomass_regeneration = "Biomass_regeneration",
        Biomass_speciesData = "Biomass_speciesData",
        # HSI_PineMarten = "HSI_PineMarten", ## used for postprocess, not devel nor production
        LandWeb_output = "LandWeb_output",
        # LandWeb_summary = "LandWeb_summary", ## used for postprocess, not devel nor production
        # NRV_summary = "NRV_summary", ## used for postprocess, not devel nor production
        scfmDriver = "scfmDriver",
        scfmEscape = "scfmEscape",
        scfmIgnition = "scfmIgnition",
        scfmLandcoverInit = "scfmLandcoverInit",
        scfmRegime = "scfmRegime",
        scfmSpread = "scfmSpread",
        timeSinceFire = "timeSinceFire"
        #visualize_LandR_output = "visualize_LandR_output" ## used for postprocess, not devel nor production
      )

      # options ------------------------------------------------------------------------------------
      private[[".options"]] <- list(
        fftempdir = file.path(dirname(tempdir()), "scratch", "BC_HRV", "ff"),
        future.globals.maxSize = 1000*1024^2, ## 1000 MiB (0.98 GiB)
        LandR.assertions = TRUE,
        LandR.verbose = 1,
        map.dataPath = self$paths$inputPath, # not used yet
        map.maxNumCores = pemisc::optimalClusterNum(20000, parallel::detectCores() / 2),
        map.overwrite = TRUE,
        map.tilePath = FALSE, ## TODO: use self$paths$tilePath once parallel tile creation works
        map.useParallel = TRUE, ## TODO: streamline useParallel: used directly for post-processing
        pemisc.useParallel = TRUE, ## TODO: streamline useParallel: used directly by scfm
        rasterMaxMemory = 5e+9,
        rasterTmpDir = normPath(file.path(self$paths[["scratchPath"]], "raster")),
        reproducible.cacheSaveFormat = "rds", ## can be "qs" or "rds"
        reproducible.conn = dbConnCache("sqlite"), ## "sqlite" or "postgresql"
        reproducible.destinationPath = normPath(self$paths[["inputPath"]]),
        reproducible.inputPaths = NULL,
        reproducible.nThreads = 2,
        reproducible.overwrite = TRUE,
        reproducible.showSimilar = TRUE,
        reproducible.useGDAL = FALSE, ## NOTE: gdal is faster, but mixing gdal with raster causes inconsistencies
        reproducible.useTerra = FALSE, ## TODO: update + test with terra
        Require.RPackageCache = "default", ## will use default package cache directory: `RequirePkgCacheDir()`
        spades.futurePlan = "callr",
        spades.memoryUseInterval = 10, ## track memory use every 10 seconds
        spades.messagingNumCharsModule = 36,
        spades.moduleCodeChecks = TRUE,
        spades.qsThreads = 4,
        spades.recoveryMode = FALSE,
        spades.scratchPath = normPath(self$paths[["scratchPath"]]),
        spades.useRequire = FALSE # Don't use Require... meaning assume all pkgs installed
      )

      # parameters ---------------------------------------------------------------------------------
      private[[".params_full"]] <- list(
        .globals = list(
          fireTimestep = 1L,
          initialB = 10,
          sppEquivCol = "BC_HRV",
          successionTimestep = 10,
          summaryInterval = 50,
          summaryPeriod = c(600, 1000),
          vegLeadingProportion = 0.8,
          .plotInitialTime = 0,
          .plots = c("object", "png", "raw", "screen"),
          .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
          .studyAreaName = self$context$studyAreaName,
          .useParallel = 2 ## doesn't benefit from more DT threads
        ),
        # ageModule = list(
        #   maxAge = 400L, ## was `maxAge` previously
        #   startTime = 0 ## sim(start)
        # ),
        BC_HRV_preamble = list(
          bufferDist = 20000,        ## 20 km buffer
          bufferDistLarge = 50000,   ## 50 km buffer
          dispersalType = "default",
          fireRegimePolysType = "BECZONE",
          friMultiple = 1L,
          landscapeUnits = dots$studyAreaName, ## the un-hashed vector of studyAreaNames
          minFRI = 25L,
          pixelSize = 125,
          treeClassesLCC = c(1:15, 20, 32, 34:36), ## should match B_bDP's forestedLCCClasses
          .plotInitialTime = 0, ## sim(start),
          .useCache = c(".inputObjects") ## faster without caching for "init"
        ),
        Biomass_borealDataPrep = list(
          biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                            (logAge + cover | ecoregionGroup))),
          dataYear = 2011,
          ecoregionLayerField = "ECOREGION", # "ECODISTRIC" ## TODO: use BEC classifications???
          forestedLCCClasses = 1:6, ## LCC2010 default
          LCCClassesToReplaceNN = numeric(0), ## LCC2010 default
          pixelGroupAgeClass = 2 * 10,  ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
          pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
          subsetDataAgeModel = 100,
          subsetDataBiomassModel = 100,
          speciesTableAreas = c("BSW", "BP", "MC", "PM"),
          speciesUpdateFunction = list(
            quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
            quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
          ),
          useCloudCacheForStats = FALSE, ## TODO: re-enable once errors in species levels resolved
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_core = list(
          growthInitialTime = 0, ## start(sim)
          initialBiomassSource = "cohortData",
          seedingAlgorithm = "wardDispersal",
          .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_regeneration = list(
          fireInitialTime = 1, ## start(sim, "year") + 1
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_speciesData = list(
          dataYear = 2011,
          omitNonVegPixels = TRUE,
          types = NULL, # using "BCVRI2011" from preamble
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        LandWeb_output = list(
          summaryInterval = 50, ## also set in .globals
          .plotInitialTime = 0 ## sim(start)
        ),
        HSI_PineMarten = list(
          ageClasses = c("Young1", "Young2", "Immature1", "Immature2", "Mature1", "Mature2", "Old", "Old2"),
          ageClassCutOffs = seq(0, 140, 20),
          ageClassMaxAge = 400L, ## was `maxAge` previously
          disturbanceAgeCutoff = 10L, ## TODO: what makes sense as default here?
          reps = 1L:10L, ## TODO: used elsewhere to setup runs (expt table)?
          studyAreaNamesCol = "LU_NAME",
          summaryInterval = 50,        ## also in .globals
          summaryPeriod = c(600, 1000), ## also in .globals
          timeSeriesTimes = 601:650,
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects") ## don't cache 'init'
        ),
        LandWeb_summary = list(
          ageClasses = c("Young1", "Young2", "Immature1", "Immature2", "Mature1", "Mature2", "Old", "Old2"),
          ageClassCutOffs = seq(0, 140, 20),
          ageClassMaxAge = 400L, ## was `maxAge` previously
          reps = 1L:10L, ## TODO: used elsewhere to setup runs (expt table)?
          simOutputPath = self$paths[["outputPath"]],
          summaryInterval = 50,        ## also in .globals
          summaryPeriod = c(600, 1000), ## also in .globals
          timeSeriesTimes = 601:650,
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          version = private[[".version"]],
          .makeTiles = FALSE, ## no tiles until parallel tile creation resolved (ropensci/tiler#18)
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects", "animation", "postprocess"), ## don't cache 'init'
          .useParallel = self$options[["map.maxNumCores"]]
        ),
        NRV_summary = list(
          ageClasses = c("Young1", "Young2", "Immature1", "Immature2", "Mature1", "Mature2", "Old", "Old2"),
          ageClassCutOffs = seq(0, 140, 20),
          ageClassMaxAge = 400L, ## was `maxAge` previously
          reps = 1L:10L, ## TODO: used elsewhere to setup runs (expt table)?
          #simOutputPath = self$paths[["outputPath"]],
          studyAreaNamesCol = "LU_NAME",
          summaryInterval = 50,        ## also in .globals
          summaryPeriod = c(600, 1000), ## also in .globals
          timeSeriesTimes = 601:650,
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          .plotInitialTime = 0, ## sim(start)
          .useCache = c(".inputObjects") ## don't cache 'init'
        ),
        scfmDriver = list(
          pMax = 0.27,
          targetN = 1000, ## increase targetN for more robust estimates, longer run-time
          .useCache = ".inputObjects", ## don't cache 'init'
          .useCloud = FALSE,
          .useParallelFireRegimePolys = TRUE
        ),
        scfmEscape = list(
          startTime = 1, ## sim(start) + 1
          .useCache = ".inputObjects" ## don't cache 'init'
        ),
        scfmIgnition = list(
          startTime = 1, ## sim(start) + 1
          .useCache = ".inputObjects" ## don't cache 'init'
        ),
        scfmLandcoverInit = list(
          sliverThreshold = 1e8, ## polygons <100 km2 are merged with closest non-sliver neighbour
          .plotInitialTime = 1, ## sim(start) + 1
          .useCache = ".inputObjects" ## don't cache 'init'
        ),
        scfmRegime = list(
          fireEpoch = c(1971, 2000), ## TODO: use longer epoch for areas too small w/ not enough fire data
          .useCache = ".inputObjects" ## don't cache 'init'
        ),
        scfmSpread = list(
          startTime = 1, ## sim(start) + 1
          .plotInitialTime = 1, ## sim(start) + 1
          .plotInterval = 5,
          .useCache = ".inputObjects" ## don't cache 'init'
        ),
        timeSinceFire = list(
          startTime = 1L,
          .useCache = ".inputObjects" ## faster without caching for "init"
        )
      )

      self$params <- private[[".params_full"]]

      invisible(self)
    },

    #' @description Update a `bcnrvConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      ## mode ---------------------------------------
      if (self$context[["mode"]] %in% c("development", "production")) {
        self$args <- list(
          cloud = list(
            useCloud = TRUE
          ),
          delayStart = if (self$context[["mode"]] == "production") delay_rnd(5L:15L) else 0L, # 5-15 minutes
          endTime = 1000,
          successionTimestep = 10,
          summaryPeriod = c(600, 1000),
          summaryInterval = 50,
          timeSeriesTimes = 601:650
        )

        self$params <- list(
          .globals = list(
            .plots = c("object", "png", "raw") ## don't plot to screen
          )
        )
      } else if (self$context$mode == "postprocess") {
        ## TODO: additional postprocessing modules
        self$modules <- list("BC_HRV_preamble", "Biomass_speciesData", "HSI_PineMarten",
                             "LandWeb_summary", "NRV_summary")
      }

      ## options -- based on mode
      self$options <- list(
        LandR.assertions = if (self$context[["mode"]] == "production") FALSE else TRUE,
        spades.moduleCodeChecks = if (self$context[["mode"]] == "production") FALSE else TRUE
      )

      ## study area + run info ----------------------
      self$params <- list(
        .globals = list(
          .studyAreaName = self$context[["studyAreaName"]]
        ),
        Biomass_borealDataPrep = list(
          pixelGroupBiomassClass = 1000 / (250 / self$context[["pixelSize"]])^2 ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
        )
      )

      ## paths --------------------------------------
      self$paths <- list(
        outputPath = .updateOutputPath(self, .bcnrvRunName),
        tilePath = file.path(.updateOutputPath(self, .bcnrvRunName), "tiles")
      )

      return(invisible(self))
    }
  ),

  private = list(
    finalize = function() {
      if (!is.null(self$options[["reproducible.conn"]])) {
        if (requireNamespace("DBI", quietly = TRUE)) {
          DBI::dbDisconnect(self$options[["reproducible.conn"]])
        }
      }
    }
  )
)
