## -------------------------------
##
## Install libraries
## 
## Purpose: 
## 
## This file installs all required packages for eGRID using the package "renv". 
## 
## Authors:  
##      Sean Bock, Abt Global
##      Teagan Goforth, Abt Global
##
## -------------------------------

if(!require("renv", character.only = TRUE)){ # installing renv package if not already
  install.packages("renv")
}

required_packages <- unique(renv::dependencies()$Package) # determining required packages used in project

# Loop through each package and install if not already installed.
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    print(paste("Package", package, "not found. Installing package!"))
    install.packages(package)
    require(package, character.only = TRUE)
  }
}