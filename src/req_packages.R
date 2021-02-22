# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2",  "sensitivity", "here", 'stats', 'optimParallel',
              'hydroPSO', 'votesys', 'DEoptim', 'pso','ppso', 'tidyverse', 'readxl',
              'here', 'igraph', 'dplyr', 'readxl', 'here', 'ggplot2','corrplot')
ipak(packages)

