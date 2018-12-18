getLibrary <- function(lib) {
  if(!lib %in% rownames(installed.packages())){
    install.packages(lib, repos = "http://cran.us.r-project.org")
  }
  library(as.character(lib), character.only = TRUE)
}