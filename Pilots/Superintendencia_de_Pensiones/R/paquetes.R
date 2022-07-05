# Package names
packages <- c("tidyverse", "ggplot2", "dplyr", "tidyr", "reshape2", "knitr", "lubridate",
              "ggpubr", "stringr", "stringi", "here", "summarytools", "magrittr", "sjPlot", "sjmisc",
              "CGPfunctions", "viridis", "grid", "ggthemes", "ggsci", "likert")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))