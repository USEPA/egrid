

# List of required packages
required_packages <- c("dplyr", 
                       "readr", 
                       "purrr", 
                       "glue",
                       "here",
                       "httr",
                       "tidyr",
                       "knitr",
                       "gt",
                       "tibble"
                       )

# Install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}