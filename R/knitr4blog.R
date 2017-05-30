# filename should have no space
# recomend you have a '_' sign instead of having a blank between
# words
# example of the use of function:
# knitr4blog("fileName.Rmd")

change_codeclass <- function(fileName, className){
  myweb <- readLines(fileName)
  myweb <- gsub("pre class=\"r\"",
                paste0("pre class=\"", className,"\""),
                myweb)
  myweb <- paste(as.character(myweb), collapse = "\n")
  write.table(myweb,
              file = fileName,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
}

knitr4blog <- function(fileName){
  rmarkdown::render(fileName)
  change_codeclass(paste0(gsub(".Rmd","", fileName),".html"),
                  "language-r")
}


