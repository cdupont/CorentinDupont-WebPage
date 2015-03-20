#Script for generating blog post containing R markdown
#Usage: ./knit <my_post>.Rmd

Rscript -e "library(knitr); knit('$1')"
