context("Rmarkdown documents run")

runAllChunks <- function(rmd, envir=new.env()){
  tempR <- tempfile(fileext = ".R")
  on.exit(unlink(tempR))
  null <- suppressMessages(knitr::knit(rmd, output=tempR, quiet = TRUE))
}


dir <- if(interactive()) "" else "../../"

# do not use file duration.rmd
# dput(list.files(path = "./inst/demos/", pattern = "\\.Rmd",
#                 full.names = TRUE, ignore.case = TRUE))
files <- c("./README.Rmd", "./inst/demos/concept.Rmd",
           "./inst/demos/framework.Rmd", "./inst/demos/import.Rmd",
           "./inst/demos/metrics.Rmd")

test_that("all R Markdown documents run", {
  for(i in paste0(dir, files)) {
    expect_silent(suppressWarnings(runAllChunks(i)))
  }
})

