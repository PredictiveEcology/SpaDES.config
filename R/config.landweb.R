#' LandWeb project configuration class
#'
#' This extends the `projConfig` class by setting various LandWeb config defaults,
#' and implements custom validation and finalizer methods.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname landwebConfig-class
landwebConfig <- R6::R6Class(
  "landwebConfig",
  inherit = projConfig,
  public = list(
    #' @description Create an new `landwebConfig` object
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments passed to `useContext()`
    #'
    initialize = function(projectPath, ...) {
      self$context <- useContext(projectName = "LandWeb", projectPath = projectPath, ...)

      ## do paths first as these may be used below
      # paths ---------------------------------------------------------------------------------------
      self$paths <- list(
        cachePath = "cache",
        inputPath = "inputs",
        inputPaths = NULL, ## aka dataCachePath
        modulePath = "m",
        outputPath = "outputs",
        projectPath = normPath(projectPath),
        scratchPath = file.path(dirname(tempdir()), "scratch", "LandWeb"),
        tilePath = file.path("outputs", "tiles")
      )

      # arguments -----------------------------------------------------------------------------------
      self$args <- list(
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
      self$modules <- list(
        Biomass_borealDataPrep = "Biomass_borealDataPrep",
        Biomass_core = "Biomass_core",
        Biomass_regeneration = "Biomass_regeneration",
        Biomass_speciesData = "Biomass_speciesData",
        LandMine = "LandMine",
        LandWeb_output = "LandWeb_output",
        LandWeb_preamble = "LandWeb_preamble",
        #LandWeb_summary = "LandWeb_summary", ## used for postprocess, not devel nor production
        timeSinceFire = "timeSinceFire"
      )

      # options ------------------------------------------------------------------------------------
      self$options <- list(
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
        spades.moduleCodeChecks = FALSE,
        spades.qsThreads = 4,
        spades.recoveryMode = FALSE,
        spades.useRequire = FALSE # Don't use Require... meaning assume all pkgs installed
      )

      # parameters ---------------------------------------------------------------------------------
      self$params <- list(
        .globals = list(
          fireTimestep = 1L,
          sppEquivCol = "LandWeb",
          successionTimestep = 10,
          summaryPeriod = c(700, 1000),
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
          summaryPeriod = c(700, 1000), ## also in .globals
          summaryInterval = 100,
          timeSeriesTimes = 601:650,
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          version = 3L
        ),
        timeSinceFire = list(
          startTime = 1L,
          .useCache = c(".inputObjects") ## faster without caching for "init"
        )
      )

      invisible(self)
    },

    #' @description Update a `LandWebConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      ## mode ---------------------------------------
      if (self$context[["mode"]] %in% c("development", "production")) {
        self$args <- list(
          cloud = list(
            useCloud = TRUE
          ),
          delayStart = if (self$context[["mode"]] == "development") 0L else sample(5L:15L, 1), # 5-15 minutes
          endTime = 1000,
          successionTimestep = 10,
          summaryPeriod = c(700, 1000),
          summaryInterval = 100,
          timeSeriesTimes = 601:650
        )

        self$params <- list(
          .globals = list(
            .plots = c("object", "png", "raw") ## don't plot to screen
          )
        )
      } else if (self$context[["mode"]] == "profile") {
        self$args <- list(
          delayStart = 0,
          endTime = 20,
          successionTimestep = 10,
          summaryPeriod = c(10, 20),
          summaryInterval = 10,
          timeSeriesTimes = 10
        )

        self$params <- list(
          .globals = list(
            .plotInitialTime = 0,
            .studyAreaName = self$context[["studyAreaName"]]
          )
        )
      } else if (self$context$mode == "postprocess") {
        self$modules <- list("LandWeb_preamble", "Biomass_speciesData", "LandWeb_summary")
      }

      ## study area + run info ----------------------
      self$params <- list(
        .globals = list(
          .studyAreaName = self$context[["studyAreaName"]]
        ),
        Biomass_borealDataPrep = list(
          pixelGroupBiomassClass = 1000 / (250 / self$context[["pixelSize"]])^2 ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
        ),
        LandMine = list(
          ROSother = switch(self$context[["ROStype"]], equal = 1L, log = log(30L), 30L)
        ),
        LandWeb_preamble = list(
          dispersalType = self$context[["dispersalType"]],
          forceResprout = self$context[["forceResprout"]],
          friMultiple = self$context[["friMultiple"]],
          pixelSize = self$context[["pixelSize"]],
          ROStype = self$context[["ROStype"]]
        )
      )

      if (grepl("FMU", self$context[["studyAreaName"]])) {
        self$params <- list(
          Biomass_borealDataPrep = list(
            biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)))
          )
        )
      } else if (grepl("provMB", self$context[["studyAreaName"]])) {
        self$params <- list(
          Biomass_speciesData = list(
            types = c("KNN", "CASFRI", "Pickell", "MBFRI")
          )
        )
      }

      if (isFALSE(self$context[["succession"]])) {
        self$modules <- list("LandWeb_preamble", "Biomass_speciesData",
                             "LandMine", "LandWeb_output", "timeSinceFire")
      }

      ## paths --------------------------------------
      self$paths <- list(
        outputPath = .updateOutputPath(self$paths[["outputPath"]], self$context),
        tilePath = file.path(.updateOutputPath(self$paths[["outputPath"]], self$context), "tiles")
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

#' @keywords internal
.updateOutputPath <- function(outputPath, context) {
  .runName <- .landwebRunName(context, withRep = FALSE)

  if (context$mode == "postprocess") {
    file.path(outputPath, .runName)
  } else {
    file.path(outputPath, .runName, sprintf("rep%02d", context[["rep"]]))
  }
}
