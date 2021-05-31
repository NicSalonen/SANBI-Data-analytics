
###########################################################################
##       Running this script will estimate the quality of a survey       ##
##      and produce a summary of all data collected for the species      ##
#        Please save and rename a copy of this template to your PC        #
###########################################################################

###########################################################################
#                       See end of script for help                        #
###########################################################################

###########################################################################
###                         Package Compilation                         ###
###########################################################################

### Compiling the SEADS package

# If the package was compiled on your PC previously, you do not need to compile
# the package again.

# Open SEADS.Rproj from the containing folder "~\Estimating Survey Efficacy\SEADS"
# First clear the environment
rm(list = ls())
# In the lower right window, select files, and you should see the contents of
# the containing folder, the necessary R functions should appear.
# Press 'ctrl+shift+b' and the package should compile. In the upper right window,
# a dialog will tell you if there are any errors ("build" tab). If there are, you
# may need to install some packages. Those that do will be listed in the error
# message.

###########################################################################
###                          Installing packages                        ###
###########################################################################

# All packages which SEADS depends on (once installed) will be activated upon
# compilation or by calling the below function:
library(SEADS)

############################################################################
############            Setting workspace variables          ##############
############################################################################
# Plant species - used for titles in plots and some filepaths
species <- 'A. viscidula'
# Path to main folder, i.e "C:/users/USER/Desktop/Estimating Survey Efficacy"
wd <- 'F:/Acacia RA work/Estimating Survey Efficacy'

# GPS data file path i.e. "C:/users/USER/Desktop/Estimating Survey Efficacy/GPS Data"
# The file path is built automatically from the main directory "wd"
filepath <- file.path(wd, 'GPS Data')
setwd(wd)

# Specify the name of the GPS track "trackfile" and waypoint file "wpfile" which
# is found in "filepath"
trackfile <- NULL# "Track Acacia adunca Bien Donne Main Site 20141021.gpx" or NULL
wpfile <- NULL# "Waypoint Acacia adunca Bien Donne" or NULL
getwd()

### Used by plotSurvey
# Modify for custom bin_widths: greater survey areas will require greater bin
# sizes due to memory.
bin_width <- 1 # in meters, must be an integer i.e. 1, 2, 5 etc
view = 2 # in number of bins = meters, must be an integer i.e. 1, 2, 3

# Google API key: <Key <- NULL> if no key exists
# If you would like to automatically produce a Google Earth Image in R, please
# see <https://cloud.google.com/maps-platform/> on how to obtain a Google API Key.
KEY <- NULL


### Specify data for dataSummary
# This file can be any data file from the invasive acacia data file-system. The
# function reads specific column headers from this set of data and will not
# work on data sets with different headings. The idea is to use this function on
# the consolidated data sets as it creates a summary on all of the data that has
# ever been collected.
# The name of the consolidated data file, these files in the invasive acacia
# data file-system are in the format "Acacia species vYYYYMMDD.csv". The path to
# the data is generated and stored as allData.

csvfile <- "Acacia viscidula v20210414.csv"
allData <- file.path(wd, 'Historic Data', csvfile)

###########################################################################
##                          Import survey data                           ##
###########################################################################

if (!is.null(trackfile)){
  track <- importSurvey(filepath, trackfile, 'track')
} else if (is.null(trackfile)){
  track <- NULL
}
if (!is.null(wpfile)){
  waypoint <- importSurvey(filepath, wpfile, 'waypoint')
} else if (is.null(wpfile)){
  waypoint <- NULL
}
###########################################################################
##                    Estimate survey site dimensions                    ##
###########################################################################

# surveyDim can take any number of imported tracks. Imported waypoints do
# not need to be passed through surveyDim.

srvydim <- surveyDim(track)

###########################################################################
##                           Plot survey data                            ##
###########################################################################

google_key()
register_google(KEY, write = FALSE)
mapcenter <- c(mean(track$lon), mean(track$lat))
surveymap <- plotSurvey(bin_width,
                        srvydim,
                        waypoint,
                        mapcenter,
                        KEY = KEY,
                        zoom = 17,
                        view = view)

###########################################################################
##                         Create summary table                          ##
###########################################################################

# all input variables are generated by the previous functions.
efficacy_table <- surveyEfficacy(srvydim, surveymap)
efficacy_table
# The path to save the survey efficacy summary table to
efftab_path <- file.path(wd, "Output Figures and Data",
                         species,
                         paste(substr(trackfile,7,(nchar(trackfile)-4)),
                               "Survey efficacy summary table.csv"))
write.csv(efficacy_table, file = efftab_path)

# Summary of all existing data for species
smry <- dataSummary(allData, species)
smry
setwd(wd)
getwd()
# The path to save the data summary table to
sumtab_path <- file.path(wd, "Output Figures and Data", species,
                         paste(substr(csvfile,1,(nchar(csvfile)-4)),
                               "summary table.csv"))
write.csv(smry, file = sumtab_path)
###########################################################################
##                           Function support                            ##
###########################################################################

# Run the below lines individually for help on each function

help("importSurvey")
help("surveyDim")
help("plotSurvey")
help("surveyEfficacy")
help('dataSummary')

###########################################################################
###########################################################################
