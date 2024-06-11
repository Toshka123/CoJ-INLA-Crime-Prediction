This script performs the analysis of an integer-valued first-order autoregressive process (INAR(1)) using the INLA package in R. Below is a summary of the script's functionality:

Installation of Required Packages
1.	Install necessary packages, including INLA, sf, rgdal, maptools, dplyr, ggplot2, tidyverse, kableExtra, sp, tmap, viridis, corrplot, gghighlight, spacetime, lubridate, MASS, lmtest, FRK, jtools, viridisLite, usethis, devtools, DClusterm, maps, and tibble.

Data Loading and Preparation

2.	Load the dataset "SAPS_quarterly_data_CoJ_average.csv".
3.	Explore the dataset and its variables.
4.	Prepare the dataset for analysis, including setting the working directory, loading shapefiles, selecting relevant variables, and creating basis functions.

Model Fitting

5.	Fit different models:
o	INLA models with AR1 and RW1 specifications.
o	Linear regression, Poisson, Quasi-Poisson, and Negative Binomial models.
6.	Assess model goodness-of-fit, including dispersion testing.

Prediction and Output
7.	Predict crime counts for a specified quarter using the fitted models.
8.	Output the predicted results to CSV files.

Visualization
9.	Visualize the predicted crime counts using scatterplots and thematic maps.
10.	Plot actual crime counts and predicted values on maps.

Additional Functionality

11.	Check distribution of crime counts.
12.	Assess dispersion in models.
13.	Merge datasets for visualization purposes.

Instructions for Use

•	Ensure all required packages are installed.

•	Set the working directory to the location of the dataset and shapefiles.

•	Run the script in R or RStudio.

•	Review the output files for predicted crime counts and visualization results
