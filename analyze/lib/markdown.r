markdown.data.frame <- function(df) {
  # http://jeromyanglim.blogspot.com/2012/05/getting-started-with-r-markdown-knitr.html

  # Header
  cat(paste(colnames(df), collapse=" | "), sep = "\n")

  # Separator
  cat(paste(rep('---', ncol(paper.table)), collapse = ' | '), sep = "\n")

  # Body
  cat(apply(df, 1, function(X) paste(X, collapse=" | ")), sep = "\n")
}
