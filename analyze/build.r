#!/usr/bin/env Rscript

library(knitr)
library(markdown)

source('src/posture.reliability.r')
source('src/pie.matrix.r')
source('src/diff.by.privacy.r')
# This takes long.
if (!('fit.participant' %in% ls())) {
  source('src/multivariate.r')
}

knit('paper.Rmd')
markdownToHTML('paper.md', 'paper.html')
# browseURL(paste('file://', file.path(getwd(), 'paper.html'), sep=''))
