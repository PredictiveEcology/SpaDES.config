#' LandWeb project configuration class
#'
#' This extends the `projConfig` class by setting various LandWeb config defaults,
#' and implements custom validation and finalizer methods.
#'
#' @export landwebConfig
#' @importFrom R6 R6Class
#' @rdname landwebConfig-class
landwebConfig <- R6::R6Class(
  "landwebConfig",
  inherit = projConfig,
  public = list(
    #' @description Create an new `landwebConfig` object
    initialize = function() {
      # paths --------------------------------------------------------------------------------------
      self$paths = list(
        cachePath = "cache",
        inputPath = "inputs",
        inputPaths = NULL, ## aka dataCachePath
        modulePath = "m",
        outputPath = "outputs",
        projectPath = ".",
        scratchPath = file.path(dirname(tempdir()), "scratch", "LandWeb"),
        tilePath = file.path("outputs", "tiles")
      )

      # arguments -----------------------------------------------------------------------------------
      self$args = list(
        cloud = list(
          cacheDir = "LandWeb_cloudCache",
          googleUser = "",
          useCloud = FALSE
        ),
        delayStart = 0,
        endTime = 1000,
        notifications = list(
          slackChannel = ""
        )
      )

      # modules ------------------------------------------------------------------------------------
      self$modules = list(
        Biomass_borealDataPrep = "Biomass_borealDataPrep",
        Biomass_core = "Biomass_core",
        Biomass_regeneration = "Biomass_regeneration",
        Biomass_speciesData = "Biomass_speciesData",
        LandMine = "LandMine",
        LandWeb_output = "LandWeb_output",
        LandWeb_preamble = "LandWeb_preamble",
        LandWeb_summary = "LandWeb_summary",
        timeSinceFire = "timeSinceFire"
      )

      # options ------------------------------------------------------------------------------------
      self$options = list(
        fftempdir = file.path(dirname(tempdir()), "scratch", "LandWeb", "ff"),
        future.globals.maxSize = 1000*1024^2,
        LandR.assertions = FALSE,
        LandR.verbose = 1,
        map.dataPath = self$paths$inputPath, # not used yet
        map.overwrite = TRUE,
        map.tilePath = self$paths$tilePath,
        map.useParallel = FALSE, ## TODO: parallel processing in map needs to be fixed!
        rasterMaxMemory = 5e+9,
        rasterTmpDir = file.path(dirname(tempdir()), "scratch", "raster"),
        reproducible.cacheSaveFormat = "rds", ## can be "qs" or "rds"
        reproducible.conn = dbConnCache("sqlite"), ## "sqlite" or "postgresql"
        reproducible.destinationPath = normPath(self$paths$inputPath),
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
        spades.moduleCodeChecks = FALSE,
        spades.qsThreads = 4,
        spades.recoveryMode = FALSE,
        spades.useRequire = FALSE # Don't use Require... meaning assume all pkgs installed
      )

      # parameters ---------------------------------------------------------------------------------
      self$params = list(
        .globals = list(
          fireTimestep = 1L,
          sppEquivCol = "LandWeb",
          successionTimestep = 10,
          vegLeadingProportion = 0.8,
          .plotInitialTime = 0,
          .plots = c("object", "png", "raw", "screen"),
          .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
          .studyAreaName = "random",
          .useCache = c(".inputObjects", "init"),
          .useParallel = 2 ## doesn't benefit from more DT threads
        ),
        Biomass_borealDataPrep = list(
          biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                            (logAge + cover | ecoregionGroup))),
          ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
          forestedLCCClasses = c(1:15, 20, 32, 34:36), ## should match preamble's treeClassesLCC
          LCCClassesToReplaceNN = 34:36,
          # next two are used when assigning pixelGroup membership; what resolution for
          #   age and biomass
          pixelGroupAgeClass = 2 * 10,  ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
          pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
          subsetDataAgeModel = 100,
          subsetDataBiomassModel = 100,
          speciesUpdateFunction = list(
            quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
            quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
          ),
          useCloudForStats = TRUE
        ),
        Biomass_core = list(
          initialBiomassSource = "cohortData",
          seedingAlgorithm = "wardDispersal",
          .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2 ## GB
        ),
        Biomass_regeneration = list(
          ## use module defaults (unless specified in .globals)
        ),
        Biomass_speciesData = list(
          omitNonVegPixels = TRUE,
          types = c("KNN", "CASFRI", "Pickell", "ForestInventory")
        ),
        LandMine = list(
          biggestPossibleFireSizeHa = 5e5,
          burnInitialTime = 1L,
          maxReburns = 20L,
          maxRetriesPerID = 4L,
          minPropBurn = 0.90,
          ROSother = 30L,
          useSeed = NULL ## NULL to avoid setting a seed, which makes all simulation identical!
        ),
        LandWeb_output = list(
          summaryInterval = 100
        ),
        LandWeb_preamble = list(
          bufferDist = 20000,        ## 20 km buffer
          bufferDistLarge = 50000,   ## 50 km buffer
          dispersalType = "default",
          friMultiple = 1L,
          pixelSize = 250,
          minFRI = 25L,
          ROStype = "default",
          treeClassesLCC = c(1:15, 20, 32, 34:36) ## should match B_bDP's forestedLCCClasses
        ),
        LandWeb_summary = list(
          ageClasses = c("Young", "Immature", "Mature", "Old"), ## LandWebUtils:::.ageClasses
          ageClassCutOffs = c(0, 40, 80, 120),                  ## LandWebUtils:::.ageClassCutOffs
          ageClassMaxAge = 400L, ## was `maxAge` previously
          reps = 1L:15L, ## TODO: used elsewhere to setup runs (expt table)?
          simOutputPath = self$paths$outputPath,
          summaryPeriod = c(700, 1000), ## TODO: this as also used in 09-pre-sim.R for outputs
          summaryInterval = 100,
          timeSeriesTimes = 601:650,
          upload = FALSE,
          uploadTo = "" ## TODO: use google-ids.csv to define these per WBI?
        ),
        timeSinceFire = list(
          startTime = 1L,
          .useCache = c(".inputObjects") ## faster without caching for "init"
        )
      )

      invisible(self)
    }
  ),
  private = list(
    finalize = function() {
      if (!is.null(self$options$reproducible.conn)) {
        if (requireNamespace("DBI", quietly = TRUE)) {
          DBI::dbDisconnect(self$options$reproducible.conn)
        }
      }
    }
  )
)

#' @keywords internal
.appendOutputPath = function(outputPath, context) {
  runName <- paste0(
    context$studyAreaName,
    if (context$dispersalType == "default") "" else paste0("_", context$dispersalType, "Dispersal"),
    if (context$ROStype == "default") "" else paste0("_", context$ROStype, "ROS"),
    if (isTRUE(context$succession)) "" else "_noSuccession",
    if (context$friMultiple == 1) "" else paste0("_fri", context$friMultiple),
    if (context$pixelSize == 250) "" else paste0("_res", context$pixelSize)
  )

  newOutputPath <- if (context$mode == "postprocess") {
    file.path(outputPath, runName)
  } else {
    if (is.na(context$rep)) warning("context$postProcessOnly is TRUE but rep is not NA")
    file.path(outputPath, runName, sprintf("rep%02d", context$rep))
  }

  return(newOutputPath)
}

#' Update LandWeb project configuration based on context
#'
#' @param config a `landwebConfig` class object
#' @param context a named list
#'
#' @export
#' @importFrom Require normPath
updateLandWebConfig <- function(config, context) {
  cnfg <- config$clone() ## TODO: what is recommended practice - work on copy of object?

  ## TODO: problems getting relative paths when studyAreaName == projectPath
  ## workaround is to specify "LandWeb" study area with suffix, e.g. "LandWeb_full"
  if (identical(context$studyAreaName, basename(config$paths$projectPath))) {
    context$studyAreaName <- paste0(context$studyAreaName, "_full")
  }

  ## mode ---------------------------------------
  if (context$mode %in% c("development", "production")) {
    cnfg$update(
      args = list(
        cloud = list(
          useCloud = TRUE
        ),
        delayStart = if (context$mode == "development") 0L else sample(5L:15L, 1), # 5-15 minutes
        endTime = 1000,
        successionTimestep = 10,
        summaryPeriod = c(700, 1000),
        summaryInterval = 100,
        timeSeriesTimes = 601:650
      ),
      params = list(
        .globals = list(
          .plots = c("object", "png", "raw") ## don't plot to screen
        )
      )
    )
  } else if (context$mode == "profile") {
    ## use specific study area if profiling
    context$studyAreaName <- "Tolko_SK" ## "random"

    cnfg$update(
      args = list(
        delayStart = 0,
        endTime = 20,
        successionTimestep = 10,
        summaryPeriod = c(10, 20),
        summaryInterval = 10,
        timeSeriesTimes = 10
      ),
      params = list(
        .globals = list(
          .plotInitialTime = 0,
          .studyAreaName = context$studyAreaName
        )
      )
    )
  } else if (context$mode == "postprocess") {
    cnfg$update(
      modules = list("LandWeb_preamble", "Biomass_speciesData", "LandWeb_summary")
    )
  }

  ## study area + run info ----------------------
  cnfg$update(
    params = list(
      .globals = list(
        .studyAreaName = context$studyAreaName
      ),
      Biomass_borealDataPrep = list(
        pixelGroupBiomassClass = 1000 / (250 / context$pixelSize)^2 ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
      ),
      LandMine = list(
        ROSother = switch(context$ROStype, equal = 1L, log = log(30L), 30L)
      ),
      LandWeb_preamble = list(
        dispersalType = context$dispersalType,
        forceResprout = context$forceResprout,
        friMultiple = context$friMultiple,
        pixelSize = context$pixelSize,
        ROStype = context$ROStype
      )
    )
  )

  if (grepl("FMU", context$studyAreaName)) {
    cnfg$update(
      params = list(
        Biomass_borealDataPrep = list(
          biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)))
        )
      )
    )
  } else if (context$studyAreaName == "provMB") {
    cnfg$update(
      params = list(
        Biomass_specieData = list(
          types = c("KNN", "CASFRI", "Pickell", "MBFRI")
        )
      )
    )
  }

  if (isFALSE(context$succession)) {
    cnfg$update(
      modules = list("LandWeb_preamble", "Biomass_speciesData",
                     "LandMine", "LandWeb_output", "timeSinceFire")
    )
  }

  ## NOTE: set user and machine contexts in the project to avoid hardcoding things
  ##       here, which will also make it easier to add users + machines as needed.

  ## paths --------------------------------------
  relOutputPath <- .getRelativePath(cnfg$paths$outputPath, cnfg$paths$projectPath)
  cnfg$update(
    paths = list(
      outputPath = .appendOutputPath(relOutputPath, context)
    )
  )

  cnfg$validate()

  return(cnfg$clone()) ## TODO: return a copy?
}
