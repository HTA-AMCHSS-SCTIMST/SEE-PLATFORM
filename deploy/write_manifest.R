# Write Posit Connect / rsconnect manifest.json for this Shiny app.
# From the rwebapp folder:
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" deploy/write_manifest.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), winslash = "/")
} else {
  getwd()
}
setwd(root)
message("Writing manifest.json in ", root)

rsconnect::writeManifest(
  appDir = root,
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)
message("Done. Publish this folder to Posit Connect.")
