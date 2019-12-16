.onAttach <- function(...) {
    packageStartupMessage("\n********************************************************")
    packageStartupMessage("NOTE: As of version 0.99.2 the struct 'dataset' object" )
    packageStartupMessage("  has been replaced with the DatasetExperiment object")
    packageStartupMessage("   and will be completely removed in a future release.")
    packageStartupMessage("********************************************************\n")
}