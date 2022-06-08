- We can use this app from the website: https://trinhxuantung.shinyapps.io/SNUBH_Lymphedema/


- Or we can run this app in personal computer by using Rstudio to open file "ui.R", then click "Run App".
Some R packages need to be installed before the first time running the App.
Run the below lines in RStudio to install those R packages:

packages <- c("shiny", "shinythemes", "shinydashboard", "caret", "randomForest", "data.table", "xlsx", "readxl", "ggpubr", "grid", "ggplot2", "ggpmisc", "dplyr")
install.packages(setdiff(packages, rownames(installed.packages())))


