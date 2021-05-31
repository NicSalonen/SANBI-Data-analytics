		|----------------------------------------------|
		|	README FILE FOR SEADS R-PROJECT        |
		|----------------------------------------------|
		|For any queries, contact nic.salonen@gmail.com|
		|----------------------------------------------|
-----------------------------------------------------------------------------------------
To install SEADS (Survey Efficacy and Data Summary) as a package onto your personal 
computer, follow the steps below:
1. Open 'SEADS.Rproj' (./src/SEADS.Rproj)
2. Open 'estimateSurveyQualtiy_template.R (./estimateSurveyQuality_template.R)
3. Open 'Files' tab in the window on the lower right panel, open the 
   and you should folder 'R' and you will see the R scripts which are in the src 
   folder.
4. Run rm(list = ls()) to ensure that your environment is empty.
5. Press 'ctrl+shift+B' to compile the SEADS package. This will import the 
   packages which SEADS requires and install the SEADS package to the R package 
   directory.
6. If you make any edits to the functions for your personal use, you must 
   recompile the SEADS package to enable the changes. 
7. Once you have installed/updated the SEADS package, you can simply call the 
   SEADS package using library(SEADS).
8. Help for the functions developed for the SEADS package can be obtained via 
   the following calls:
	help("importSurvey")
	help("surveyDim")
	help("plotSurvey")
	help("surveyEfficacy")
	help('dataSummary')
-----------------------------------------------------------------------------------------

The following folders are in the 'Estimating Survey Efficacy' directory:
	1. GPS Data - contains cleaned tracks and waypoints
	2. Historic Data - contains consolidated acacia data for all species
	3. Output Figures and Data - tables from the R model are 
	   automatically saved here. Figures are manually saved to this location
	4. SEADS - the directory which contains the contents to compile the
		   SEADS package

			|----------------------------------------------|
