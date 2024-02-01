# Import ADNI Dataset
install.packages("./ADNIMERGE_0.0.1.tar.gz", repos = NULL, type = "source")
library(ADNIMERGE)
data(adnimerge)

# Summary
remotes::install_github("dcomtois/summarytools", build_vignettes = TRUE)
library(summarytools)
dfSummary(adnimerge)

