#-------------------------------------------------------------------------------
# RMV2.0 (version 1.1.0)
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
#-------------------------------------------------------------------------------
################################################################################
#                         Load used R packages                                 #
################################################################################

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(DT)
library(RMV2.0)

source('js.R')


'%nin%' <- Negate('%in%')

volumes = c(getVolumes()(), "HOME"="~")

# shinyFiles package has some incompatibilities with R v3.4.1.
# A fix proposed in :
# github.com/gregorbj/VisionEval/commit/d3251e88a736f59640800deca8a89ad65f23d2df
# overrides file.info() (base package) and fileGetter() (shinyFiles package) functions
# when R is >= v3.4

if(R.version$minor >= 4){
  fileGetter <-  function (roots, restrictions, filetypes, hidden = FALSE)
  {
    if (missing(filetypes))
      filetypes <- NULL
    if (missing(restrictions))
      restrictions <- NULL
    function(dir, root) {#browser()
      currentRoots <- if (class(roots) == "function")
        roots()
      else roots
      if (is.null(names(currentRoots)))
        stop("Roots must be a named vector or a function returning one")
      if (missing(root))
        root <- names(currentRoots)[1]
      fulldir <- file.path(currentRoots[root], dir)
      writable <- as.logical(file.access(fulldir, 2) == 0)
      files <- list.files(fulldir, all.files = hidden, full.names = TRUE,
                          no.. = TRUE)
      files <- gsub(pattern = "//*", "/", files, perl = TRUE)
      if (!is.null(restrictions) && length(files) != 0) {
        if (length(files) == 1) {
          keep <- !any(sapply(restrictions, function(x) {
            grepl(x, files, fixed = T)
          }))
        }
        else {
          keep <- !apply(sapply(restrictions, function(x) {
            grepl(x, files, fixed = T)
          }), 1, any)
        }
        files <- files[keep]
      }
      fileInfo <- (file.info(files))
      fileInfo$filename <- basename(files)
      fileInfo$extension <- tolower(tools::file_ext(files))
      validIndex <- which(!is.na(fileInfo$mtime))
      fileInfo <- fileInfo[validIndex,]
      fileInfo$mtime <- format(fileInfo$mtime, format = "%Y-%m-%d-%H-%M")
      fileInfo$ctime <- format(fileInfo$ctime, format = "%Y-%m-%d-%H-%M")
      fileInfo$atime <- format(fileInfo$atime, format = "%Y-%m-%d-%H-%M")
      if (!is.null(filetypes)) {
        matchedFiles <- tolower(fileInfo$extension) %in%
          tolower(filetypes) & fileInfo$extension != ""
        fileInfo$isdir[matchedFiles] <- FALSE
        fileInfo <- fileInfo[matchedFiles | fileInfo$isdir,
                             ]
      }
      rownames(fileInfo) <- NULL
      breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
      list(files = fileInfo[, c("filename", "extension", "isdir",
                                "size", "mtime", "ctime", "atime")],
                                writable = writable,
                                exist = file.exists(fulldir),
                                breadcrumps = I(c("",breadcrumps[breadcrumps != ""])),
                                roots = I(names(currentRoots)),
                                root = root)
    }
  }
  unlockBinding("fileGetter", getNamespace("shinyFiles"))
  assign("fileGetter",fileGetter,getNamespace("shinyFiles"))

  file.info <- function (..., extra_cols = TRUE)
  {
    suppressWarnings(res <- .Internal(file.info(fn <- c(...), extra_cols)))
    res$mtime <- .POSIXct(res$mtime)
    res$ctime <- .POSIXct(res$ctime)
    res$atime <- .POSIXct(res$atime)
    class(res) <- "data.frame"
    attr(res, "row.names") <- fn
    res
  }
  unlockBinding("file.info", getNamespace("base"))
  assign("file.info",file.info,getNamespace("base"))
}
  ##########################
