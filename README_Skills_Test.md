
# Skills Test: Data Manipulation, Cleaning, and Plotting

## Overview
This project demonstrates skills in data manipulation, cleaning, and plotting using R. The included script provides a comprehensive approach to handling messy datasets and creating insightful visualizations.

## File Included
- `Skills Test - Data Manipulation Cleaning and Plotting.R`: An R script that showcases:
  - **Data Cleaning**: Steps to clean and preprocess raw data.
  - **Data Manipulation**: Transforming datasets for analysis.
  - **Data Visualization**: Generating meaningful plots to visualize trends and distributions.

## How to Use

### Prerequisites
Ensure you have R and RStudio installed. Additionally, install the required packages:
```r
install.packages(c("ggplot2", "dplyr", "tidyr"))
```

### Running the Script
1. Open the file `Skills Test - Data Manipulation Cleaning and Plotting.R` in RStudio.
2. Run the script line by line or use:
   ```r
   source("Skills Test - Data Manipulation Cleaning and Plotting.R")
   ```

## Example Highlights
### Data Cleaning
The script includes steps such as:
- Handling missing values.
- Formatting date and time columns.
- Removing duplicates.

### Data Visualization
The script creates plots such as:
- Bar plots, histograms, and scatter plots to visualize trends and distributions.

### Code Snippet
Here’s a preview of the code:
```r
# DO NOT CHANGE THE PRE-WRITTEN CODE EXCEPT WHERE MARKED
# load package
library(tidyverse)
# Add any additional packages required here
library(dplyr)
library(lubridate)
library(ggplot2)


# set seed - PLEASE EDIT --------------------------------
# CHANGE THE VALUE OF 1 TO A NUMBER OF YOUR CHOICE
set.seed(12) 

################################################################################
# code to create two datasets to work with - DO NOT EDIT
# NOTE - comments have been removed
val_create <-   # Showing the first 500 characters of the script
```

## Requirements
- R version: [insert R version]
- Required packages:
  ```r
  install.packages(c("ggplot2", "dplyr", "tidyr"))
  ```

## License
[Specify your license, e.g., MIT License or GNU GPL]

## Contact
For any questions or issues, contact [Your Name or Email].
