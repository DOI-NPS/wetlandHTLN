#' @title importData: Import tables from HTLN Wetlands database
#'
#' @importFrom dplyr filter left_join select
#'
#' @description This function imports tables from the Heartland Network database and compiles flat files
#' to be incorporated into the data package. You can either set a data source name (DSN) called "HTLN_wetlands"
#' to import the data, or can specify a file name. Eventually this will be enabled to import the data package
#' csvs from IRMA.
#'
#' @param type Select how to import the database tables.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc is not specified, will default to HTLN_wetlands.
#' Using this argument requires that you have a User DSN named HTLN_wetlands that points to the
#' database containing the latest data.}
#' \item{"dbfile"}{A specified database containing the database tables. If selected,
#' must provide the database filepath in the filepath argument.}
#' \item{"csv"}{*NOT CURRENTLY ENABLED*. Imports the csv version of the data package views. If selected, must provide the
#' filepath for the csvs in the filepath argument. This option that does not require MS Access and
#' ODBC driver on your computer. }
#' \item{"zip"}{Imports the csv versions of the data package views, as a zipped file. If selected,
#' must provide the filepath and name of the zip file. This option that does not require MS Access and
#' ODBC driver on your computer.}
#' }
#'
#' @param odbc DSN of the database when using type = DSN. If not specified will default to "HTLN_wetlands",
#' which should represent the HTLN wetland database with the latest data.
#'
#' @param filepath Quoted filepath where data package database (if type = "dbfile") or the csvs
#' (if type = "csv" or type = "zip") live.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in HTLN_wetlands environment. If \code{FALSE}, stores views in global environment
#'
#' @param data_type Select type of data to import
#' \describe{
#' \item{"vibi"}{Default. Import only VIBI-related tables. }
#' \item{"oram"}{*NOT CURRENTLY ENABLED* Import only ORAM-related tables.}
#' \item{"all"}{*NOT CURRENTLY ENABLED* Import all tables relevant for data package.}
#' }
#'
#' @param export Logical. If TRUE, will export a zip file of csvs to specified export_path.
#'
#' @param export_path Quoted string to export zipped csvs to if export = TRUE. If not specified, will export to the working directory.
#'
#' @examples
#' \dontrun{
#'
#' # Import views using DSN and export zip of views to working directory
#' importData(type ='DSN', odbc = "HTLN_wetlands", export = T) # this is the same as importData()
#'
#' # Import views from specified database:
#' importData(type ='dbfile',
#'   filepath = './HTLN_wetlands/HTLNwetlands3.4.6.accdb')
#'
#' }
#'
#' @return HTLN wetlands views in specified environment
#'
#' @export
#'

importData <- function(type = 'DSN', odbc = "HTLN_wetlands", filepath = NA, new_env = TRUE, data_type = "vibi",
                       export = FALSE, export_path = NA){

  # Appends DWC columns `type` and `basisOfRecord` to a dataset
  mini_darwin_core <- function(df) {
    df$type <- "Event"
    df$basisOfRecord <- "HumanObservation"

    df
  }

  #---- Bug handling ----
  # match arguments
  type <- match.arg(type, c("DSN", "dbfile", "csv", "zip"))
  stopifnot(class(new_env) == 'logical')
  data_type <- match.arg(data_type, c("vibi")) #+++ Add oram and all when they're enabled ++++
  stopifnot(is.logical(export))

  # check that filepath was specified for non-DSN options
  if(type %in% c("dbfile", "csv", "zip")){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when type = '",
                                    type, "' option."))}
  }

  if(export == TRUE){
    if(is.na(export_path)){
      export_path <- getwd()
      print(paste0("No export_path specified. Output saved to working directory: ", getwd()), quote = FALSE)}
    if(!grepl("/$", export_path)){export_path <- paste0(export_path, "/")} # add / to end of path if doesn't exist
    if(!dir.exists(export_path)){stop("Specified export_path directory does not exist.")}
    # Normalize filepath for zip
    export_pathn <- normalizePath(export_path)
  }

  if(type == 'csv'){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when type = 'csv'"))
    } else if(!file.exists(filepath)){
      stop(paste0("Specified file path does not exist. ",
                  ifelse(grepl("sharepoint", filepath), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}
    if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")}} # add / to end of filepath if doesn't exist

  # Check if type = 'csv' was specified, but .zip file is filepath
  if(type == 'csv' & grepl(".zip", filepath)){stop("Specified a zip file in filepath. Must use type = 'zip' instead of 'csv'.")}

  # check for required packages for certain arguments
  if(!requireNamespace("odbc", quietly = TRUE) & type %in% c('DSN', 'dbfile')){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE) & type %in% c('DSN', 'dbfile')){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Create new environment if new_env = T or set env as Global
  if(new_env == TRUE){HTLN_wetlands <<- new.env()}

  env <- if(new_env == TRUE){HTLN_wetlands} else {.GlobalEnv}

  if(data_type %in% c("vibi", "all")){
  tbls <- c("tbl_BigTrees",
            "tbl_Locations",
            "tbl_SamplingEvents",
            "tbl_SamplingPeriods",
            "tbl_SiteWPs",
            "tbl_VIBI_Herb",
            "tbl_VIBI_Herb_Biomass",
            "tbl_VIBI_Woody",
            "tlu_DomVeg",
            "tlu_HGMClass",
            "tlu_CoverClass",
            "tlu_WetlndSpeciesList", #"tlu_WetlndSpeciesList"
            "tlu_WoodyPlants")

  tbl_names <- gsub("tbl_", "", tbls)
  #tbl_names[tbl_names == "tlu_WetlndSpeciesList_2025_update"] <- "tlu_WetlndSpeciesList" # delete after tlu updated

  # Checks on database import to return meaningful errors on fail
  # make sure db is on dsn list if type == DSN
  if(type == "DSN"){
    dsn_list <- odbc::odbcListDataSources()
    if(!any(dsn_list$name %in% odbc)){
      stop(paste0("Specified DSN ", odbc, " is not a named database source." ))}
  }

  # Test DSN database connection and name db if successful
  if(type == "DSN"){
    tryCatch(
      db <- DBI::dbConnect(drv = odbc::odbc(), dsn = odbc),
      error = function(e){stop(paste0("Unable to connect to specified DSN."))})
  }

  # Test dbfile database connection and name db if successful
  if(type == 'dbfile'){
    tryCatch(
      db <- DBI::dbConnect(drv = odbc::odbc(),
                           .connection_string =
                             paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", filepath)),
      error = function(e){stop(paste0("Unable to connect to specified database."))})
  }

  #---- Import data package views ----
  #----- Database import -----
  if(type %in% c("DSN", "dbfile")){

    pb = txtProgressBar(min = 0, max = length(tbls), style = 3)

    tbl_import <- lapply(seq_along(tbls),
                         function(x){
                           setTxtProgressBar(pb, x)
                           tab1 <- tbls[x]
                           tab <- DBI::dbReadTable(db, tab1)
                           return(tab)
                         })

    DBI::dbDisconnect(db)

    # Name tbl_import list of tables
    tbl_import <- setNames(tbl_import, tbl_names)

    # Add list of tables in tbl_import to specified environment
    list2env(tbl_import, envir = env) # all tables into fxn env
    # Close progress bar
    close(pb)
  }

  #---- Compile VIBI views ----
  tluDom <- get("tlu_DomVeg", envir = env)
  tluHGM <- get("tlu_HGMClass", envir = env)
  tluWoody <- get("tlu_WoodyPlants", envir = env)

  tluSpecies <- get("tlu_WetlndSpeciesList", envir = env) |> unique()
  names(tluSpecies)[names(tluSpecies) == "SCIENTIFIC_NAME"] <- "ScientificName"
  names(tluSpecies)[names(tluSpecies) == "TYPE"] <- "LIFEFORM"

  tluCover <- get("tlu_CoverClass", envir = env)

  loc1 <- get("Locations", envir = env)
  loc2 <- dplyr::left_join(loc1, tluHGM, by = "HGM_ID")
  loc3 <- dplyr::left_join(loc2, tluDom, by = c("DomVegID" = "DomVeg_ID"))
  #loc3$FeatureTypes[loc3$FeatureTypes == "VIBIPlotID"] <- "VIBIplotID"
  loc3$DomVeg_Lev1_orig <- loc3$DomVeg_Lev1 # keep backup, for QC checking
  loc4 <- loc3 |> dplyr::select(-DomVeg_Lev1)

  # Fill in missing DomVeg_Lev1 using X1ofPlants
  #table(loc$X1oPlants, loc$DomVeg_Lev1)
  veg_tbl <- data.frame(X1oPlants = c("PEM", "PFO", "PSS"), DomVeg_Lev1 = c("emergent", "forest", "shrub"))
  loc5 <- left_join(loc4, veg_tbl, by = c("X1oPlants"))
  loc6 <- loc5 |> filter(FeatureTypes %in% "VIBIplotID")

  keep_cols <- c("WetlandName", "LocationID", "FeatureTypes", "FeatureID", "Park",
                 "County", "TotalMods", "IntensMods", "PlotConfig", "AreaHA",
                 "X1oPlants", "Centerline", "X1oHGM", "Directions", "Latitude",
                 "Longitude", "SampleType", "AccessPermission", "WetlandSize",
                 "EstablishmentDate", "DomVegID", "HGM_ID", "SurveyType", "Mod_Desc",
                 "DomVeg_Lev2", "DomVeg_Lev3", "DomVeg_Lev1_orig", "DomVeg_Lev1")

  loc <- loc6[, keep_cols]

  samp_pds <- get("SamplingPeriods", envir = env)
  samp_evs <- get("SamplingEvents", envir = env)

  samp_comb <- full_join(samp_pds, samp_evs, by = c("PeriodID"), suffix = c("_pd", "_ev"))
  samp_comb$PeriodDate = as.Date(sub("CUVAWetlnd", "", samp_comb$PeriodID), format = "%Y%b%d")
  samp_comb$PeriodYear = as.numeric(format(samp_comb$PeriodDate, format = "%Y"))
  samp_comb$SampleDate = as.Date(sub("CUVAWetlnd", "", samp_comb$EventID), format = "%Y%b%d")
  samp_comb$SampleYear = as.numeric(format(samp_comb$SampleDate, format = "%Y"))

  bmass <- get("VIBI_Herb_Biomass", envir = env)
  bmass$SampleDate = as.Date(sub("CUVAWetlnd", "", bmass$EventID), format = "%Y%b%d")
  bmass$SampleYear = as.numeric(format(bmass$SampleDate, format = "%Y"))
  names(bmass)[names(bmass) == "Module"] <- "ModuleNo"
  names(bmass)[names(bmass) == "DryWt"] <- "DryWt_g"

  herb1 <- get("VIBI_Herb", envir = env)
  herb1$SampleDate = as.Date(sub("CUVAWetlnd", "", herb1$EventID), format = "%Y%b%d")
  herb1$SampleYear = as.numeric(format(herb1$SampleDate, format = "%Y"))
  names(herb1)[names(herb1) == "ModNo"] <- "ModuleNo"
  herb2 <- left_join(herb1, tluCover, by = c("CovCode" = "CoverClass"))

  woody <- get("VIBI_Woody", envir = env)
  woody$SampleDate = as.Date(sub("CUVAWetlnd", "", woody$EventID), format = "%Y%b%d")
  woody$SampleYear = as.numeric(format(woody$SampleDate, format = "%Y"))
  names(woody)[names(woody) == "Module_No"] <- "ModuleNo"

  btrees <- get("BigTrees", envir = env)
  btrees$SampleDate = as.Date(sub("CUVAWetlnd", "", btrees$EventID), format = "%Y%b%d")
  btrees$SampleYear = as.numeric(format(btrees$SampleDate, format = "%Y"))
  names(btrees)[names(btrees) == "ModNo"] <- "ModuleNo"
  names(btrees)[names(btrees) == "DBH"] <- "DBH_cm"

  # Column names to order by/include for each view
  loc_cols <- c("LocationID", "FeatureID", "Park", "County", "PeriodYear",
                "SampleDate", "SampleYear", "Latitude", "Longitude",
                "TotalMods", "IntensMods", "PlotConfig",
                "AreaHA", "X1oPlants", "X1oHGM", "DomVegID", "HGM_ID",
                "Mod_Desc", "DomVeg_Lev1", "DomVeg_Lev2", "DomVeg_Lev3", "SurveyType")

  spp_cols <- c("ScientificName", "COMMON_NAME", "AUTHORITY", "FAMILY", "ACRONYM", "COFC",
                "FN", "WET", "FORM", "HABIT", "SHADE", "USDA_ID",
                "OH_TORE", "LIFEFORM",
                "OH_STATUS", "EMP", "MW", "NCNE", "NOTES")

  # Biomass
  bmass1 <- dplyr::left_join(loc, bmass, by = "LocationID", suffix = c("_Loc", "_bmass")) |>
    dplyr::filter(FeatureTypes %in% c("VIBIplotID")) |>
    dplyr::filter(!is.na(EventID))

  bmass2 <- full_join(samp_comb[,c("PeriodID", "EventID", "PeriodDate", "PeriodYear")], bmass1, by = c("EventID"))
  bmass3 <- bmass2[!is.na(bmass2$LocationID),]
  bmass_final <- bmass3[,c(loc_cols, "VIBI_Herb_Biomass_ID", "ModuleNo", "Corner", "DryWt_g", "EventID", "PeriodID")] |>
    mini_darwin_core()

  # Herbs
  herb3 <- dplyr::left_join(loc, herb2, by = "LocationID", suffix = c("_Loc", "_Herb")) |>
    dplyr::filter(FeatureTypes %in% c("VIBIplotID")) |>
    dplyr::filter(!is.na(EventID))

  herb4 <- dplyr::left_join(herb3, tluSpecies, by = c("Species" = "ScientificName"))
  names(herb4)[names(herb4) == "Species"] <- "ScientificName"
  herb4$COFC <- as.numeric(herb4$COFC)

  herb5 <- full_join(samp_comb[,c("PeriodID", "EventID", "PeriodDate", "PeriodYear")], herb4, by = c("EventID"))
  herb6 <- herb5[!is.na(herb5$LocationID),]
  herb_final <- herb6[,c(loc_cols, spp_cols, "ModuleNo", "CovCode", "MidPoint", "EventID", "PeriodID")] |>
    mini_darwin_core()

  # Woody
  woody1 <- dplyr::left_join(loc, woody, by = c("LocationID"), suffix = c("_Loc", "_Woody")) |>
    dplyr::filter(FeatureTypes %in% c("VIBIplotID")) |>
    dplyr::filter(!is.na(EventID))
  woody2 <- dplyr::left_join(woody1, tluSpecies, by = c("Scientific_Name" = "ScientificName"))
  woody3 <- dplyr::left_join(woody2, tluWoody, by = "DiamID")

  # rename cols so consistent with herb view
  names(woody3)[names(woody3) == "Scientific_Name"] <- "ScientificName"
  names(woody3)[names(woody3) == "FeatureID_Loc"] <- "FeatureID"

  woody4 <- full_join(samp_comb[,c("PeriodID", "EventID", "PeriodDate", "PeriodYear")], woody3, by = c("EventID"))
  woody5 <- woody4[!is.na(woody4$LocationID),]
  woody_final <- woody5[,c(loc_cols, spp_cols, "ModuleNo", "DiamID", "SortOrder", "DiamVal", "DBH_MidPt", "Count", "EventID", "PeriodID")] |>
    mini_darwin_core()

  # BigTrees
  btrees1 <- dplyr::left_join(loc, btrees, by = c("LocationID"), suffix = c("_Loc", "_BT")) |>
    dplyr::filter(FeatureTypes == "VIBIplotID")
  names(btrees1)[names(btrees1) == "Scientific_Name"] <- "ScientificName"
  btrees2 <- dplyr::left_join(btrees1, tluSpecies, by = "ScientificName") |>
    filter(!is.na(EventID)) # dropping records with no bigtree data

  btrees3 <- full_join(samp_comb[,c("PeriodID", "EventID", "PeriodDate", "PeriodYear")], btrees2, by = c("EventID"))
  btrees4 <- btrees3[!is.na(btrees3$LocationID),]
  btrees_final <- btrees4[,c(loc_cols, spp_cols, "ModuleNo", "DBH_cm", "EventID", "PeriodID")] |>
    mini_darwin_core()

  # remove all but final tables from HTLN_wetlands env.

  rm(list = ls(envir = env), envir = env)

  assign("locations", loc, envir = env)
  assign("biomassVIBI", bmass_final, envir = env)
  assign("herbVIBI", herb_final, envir = env)
  assign("woodyVIBI", woody_final, envir = env)
  assign("bigtreesVIBI", btrees_final, envir = env)
  assign("tluSpecies", tluSpecies, envir = env)

  # Print message in console
  print(ifelse(new_env == TRUE,
               paste0("Import complete. Views are located in HTLN_wetlands environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)
  }

  if(export == TRUE){
    dir.create(tmp <- tempfile())
    dbtbls <- names(HTLN_wetlands)
    dbtbls_name <- paste0("HTLN_wetlands_", dbtbls)

    invisible(lapply(seq_along(dbtbls), function(x){
      temp_tbl = get(dbtbls[x], envir = env)
      write.csv(temp_tbl,
                paste0(tmp, "\\", dbtbls_name[x], ".csv"),
                row.names = FALSE)
    }))

    file_list <- list.files(tmp)
    zip_name = paste0("HTLN_Wetland_VIBI_views_", format(Sys.Date(), "%Y%m%d"), ".zip")

    zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
              root = tmp,
              files = file_list)
    # csvs will be deleted as soon as R session is closed b/c tempfile
    noquote(paste0('Export complete. Wetland VIBI views saved to: ', export_pathn, "\\", zip_name))

  }
  }
