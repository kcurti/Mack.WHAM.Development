### Install a specific commit of WHAM, hopefully using Pak


### Option 1: Pak

# Install "pak" package if not already installd
install.packages("pak")
install.packages("mnormt")  # Need for plot wham function
install.packages("Hmisc")  # Need for plot wham function

# Use the "pak" package to install the dev branch of wham:
pak::pkg_install("timjmiller/wham@devel", lib="C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_devel_6146b5d")
# Ran into issue installing rcpp so had to install it separately and then rerun the wham install line
install.packages("Rcpp", lib = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_devel_6146b5d")
pak::pkg_install("timjmiller/wham@devel", lib="C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_devel_6146b5d")
library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_devel_6146b5d")


# Reinstall to streamline directory name
pak::pkg_install("timjmiller/wham@6146b5d", lib="C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_6146b5d")
library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_6146b5d")

# Install new commit (23 Jan 2025)
pak::pkg_install("timjmiller/wham@168117c", lib="C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_168117c")
library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_168117c")

### To avoid having to add different packages to the main library folder, may have to add the specific wham commit folder to the list of library paths using .libPaths()
.libPaths()



### Option 2: Devtools

# # Install devtools package
# install.packages("devtools")
# 
# # Use devtools to install
# devtools::install_github("timjmiller/wham@devel", ref = "e93b610", dependencies=TRUE, INSTALL_opts=c("--no-multiarch"))


### If need to remove a package
# remove.packages("TMB")


