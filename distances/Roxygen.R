if (!require(roxygen2)) install.packages("roxygen2")
library(roxygen2)
if (!require(tools)) install.packages("tools")
library(tools)
if (!require(this.path)) install.packages("this.path")
library(this.path)
setwd(dirname(this.path()))

code <- paste(readLines("Distances.R"), collapse = "\n")

roc <- rd_roclet()
rd_objekte <- roc_proc_text(roc, code)
writeLines(format(rd_objekte[[1]]), con = "Class_Header.Rd")
Rd2HTML(Rd = "Class_Header.Rd", out = "Class_Header.html")
