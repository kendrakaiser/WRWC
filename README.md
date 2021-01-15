# WRWC Predictive Streamflow Model
Welcome to the Wood River Water Collaborative code base for predicting annual streamflow and water rights curtailment dates!
This modeling tool automatically downloads input data and predicts total irrigation season (April- Sept) flow, and curtailment dates for the Big Wood River at Hailey and at Stanton Crossing, Camas Creek, and Silver Creek at Sportsman's Access. The suite of predictive models are multivariate regression models, meaning they are all statistically based rather than processed based. This means that the model predictions are soley based on historical relationships. While this makes for efficient model runs, if something changes significantly in a given watershed (e.g. fire in the headwaters), the model will not be able to predict streamflow conditions as well. Additional model details are described in the `Model_Details.pdf`.

# Getting Started
Download code zip file by clicking on the green code button and save in a convineient place on your computer. You will need to enter this file path into the scripts to run the code locally.

Install RStudio on your computer https://rstudio.com/products/rstudio/download/

Once RStudio is installed you will need to install a suite of packages to make the model run. You can install them by running the following line of code in the 'console'.

```
 install.packages(c('MASS', 'plotrix', 'mvtnorm', 'tidyverse', 'dataRetrieval', 'snotelr', 'XML', 'httr', 'dplyr', 'devtools', 'plyr', 'readr', 'lubridate', 'lfstat', 'gridExtra', 'varhandle', 'viridis'))
 ```
 
 One package, 'RNRCS', is pulled directly from the developers github repo, to install this package run 
 
 ```
 devtools::install_github(repo = "rhlee12/RNRCS", subdir = "/RNRCS/", force =TRUE)
 ```
 ## Setup file directories
 There are two sets of file paths for the project. The first are the paths to where you saved the gitHub code files, and the second set is a local folder where final datasets and .csv files will be saved. The github file directories contain the code and a folder for output figures, these figures need to be in the same directory as the `.Rmd` file in order for the model run report to compile correctly. The local folder where output is saved will be overwritten every time the model is run, so if saving model outputs is of interest, create another subfolder with a naming convention like '2021-02-01_ModelRun_lastname' where you copy and paste model outputs.
 
 You will need to create the following set of subfolders in the local folder for data to be saved to: 
 
![alt text ><](https://github.com/kendrakaiser/WRWC/blob/master/figures/local_file_dir.png?raw=true)

 # Run the Models
 
 The only script you need to open to run the models is `run_models.R`. In this script you will modify the file paths and run date as described below.
 
## File paths
There are two sets of file paths for the project. The first are the paths to where you saved the gitHub code files, in the example: `'~/github/WRWC/`, and the second set is the local folder where final datasets and .csv files will be saved. 

```
# GitHub File Paths
git_dir <<- '~/github/WRWC'
fig_dir <<- file.path(git_dir, 'figures') # with proper file drectory set up this will not need to be changed
input_dir <<- file.path(git_dir, 'input') # do not change

# Local File Paths
cd <<- '~/Desktop/WRWC'
data_dir <<- file.path(cd, 'data') # do not change
```

## Run Date and Prediction Year

The  prediction year is the year of interest, and the 'run_date' is the date that you are running the models. In this case, there is a different set of models that are run for February / March / April 1. The optional name inputs are provided, any other naming convention will not work (e.g. March1 or mar1 will not work, only 'march1'. The models cannot be run prior to the run date (e.g. 'feb1' 2021) because the predictor variables will not be available yet.

```
# set prediction year
pred.yr <<- 2019
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'april1'
# set end date for AgriMet Data download
end_date <<- '2020-10-01'

```
 ## Information for Model Run Report
 
 This information is printed at the top of the model run report and is valuable for tracking model outputs over time.
 
 ```
 # info for model run report
author = "Kendra Kaiser"
todays_date = "01/14/2021"
```

# Model Support
If you run the model and an error occurs, the process for getting help is to raise an 'issue', you can do this by following these steps https://docs.github.com/en/free-pro-team@latest/github/managing-your-work-on-github/creating-an-issue. This will automatically send me an email so that I can help resolve the issue. 


