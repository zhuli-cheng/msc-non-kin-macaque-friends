This directory contains input data and R scripts that are needed to reproduce the results and figures in this publication: 

It also contains an output folder produced from running the scripts on the input data. 

The workflow is as follows. However, It is unnecessary to run the scripts one by one based on this workflow. Within each script, it is made explicit the other scripts and input data that it is dependant on. You do need to make sure that the directory is downloaded as one singular folder and that the R packages are installed on an R platform of a compatible version. 

Workflow: 
1.  LoadRpackages.R
2.  ChooseSubjects.R
3.  CalculateRelatedness.R (and PlotRelatedness.R for relatedness heatmaps)
4.  CalculateDSI.R  (and PlotSocialNetworks.R)
Results from CalculateDSI.R will save into the folder: ./output/DSI. This folder contains wrangled data after the above steps in the workflow. The folder is created to avoid performing data wrangling repetitively, when we perform downstream analyses. 

5. PrepareDatasetForModels.R 
6. Analyses
    1. Q1.R
    2. Q2.R
    3. Q3.R
    4. Appendices.R

Note: the input folder and output/DSI folder are not uploaded on the GitHub page. Instead, analyses can be run using the output/DSI_scrambled.csv file. 