# Simple script


# Load SpaDES
library("SpaDES")
library("raster")

dayOfRecording <- today <- toupper(format(Sys.time(), "%d%b%y"))
dayOfRecording <- "19MAR19"

isTEST <- TRUE

if (isTEST){
  dayOfRecording <- "19MAR19"
}

# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))

# Set a storage project folder
workDirectory <- getwd()
message("Your current temporary directory is ", tempdir())

create <- if (isTEST) NULL else TRUE 
checkInputPath <- tryCatch(checkPath(file.path(dirname(dirname(getwd())), "outputs", dayOfRecording)), error = function(e) NULL)
if (is.null(checkInputPath)){
  inpPath <- tempdir()
} else {
  inpPath <- checkInputPath
}

checkOutputPath <- tryCatch(checkPath(file.path(dirname(dirname(getwd())), "outputs", today), create = create), error = function(e) NULL)
if (is.null(checkOutputPath)){
  outPath <- tempdir()
} else {
  outPath <- checkOutputPath
}

setPaths(modulePath = file.path(dirname(getwd())), 
         inputPath = inpPath, outputPath = outPath)
getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 3)

parameters <- list(
  .progress = list(type = "text", interval = 1), # for a progress bar
  birdsNWT = list(
    "baseLayer" = 2005,
    "overwritePredictions" = TRUE,
    "useTestSpeciesLayers" = TRUE, # Set it to false when you actually have results from LandR_Biomass simulations to run it with
    "useParallel" = FALSE, # Using parallel in windows is currently not working.
    "predictionInterval" = 1
  )
)

# Passing the uplandsRaster here makes sure that all computers can use it as the operations take up a lot of memory
uplandsRaster <- prepInputs(targetFile = "uplandsNWT250m.tif", 
                            url = "https://drive.google.com/open?id=1EF67NCH7HqN6QZ0KGlpntB_Zcquu6NJe", 
                            destinationPath = getPaths()$inputPath, filename2 = NULL)

# Check the list of species available:
showAvailableBirdSpecies()

.objects <- list(
  "uplandsRaster" = uplandsRaster)
modules <- list("birdsNWT")
inputs <- list()
outputs <- list()

birdsNWT <- simInitAndSpades(times = times, params = parameters, modules = modules,
                             objects = .objects, debug = 2)