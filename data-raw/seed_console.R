# Create/refresh HTA demo study plus dummy expert judgments for SHELF testing.
# Requires Mongo (Atlas or local) and packages installed.
#   source("data-raw/seed_console.R")

if (file.exists(".Renviron")) readRenviron(".Renviron")
r_dir <- "R"
for (f in c("config.R", "log.R", "ids.R", "mongo.R", "auth.R", "chips.R", "shelf.R", "studies.R", "seed.R")) {
  source(file.path(r_dir, f))
}
u <- user_as_list(dev_login("admin@sctimst.ac.in", "Platform Admin", "admin"))
print(seed_demo(u))
