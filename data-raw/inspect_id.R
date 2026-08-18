readRenviron(".Renviron")
for (f in c("config.R", "log.R", "ids.R", "mongo.R")) source(file.path("R", f))
doc <- mongo_insert("organizations", list(
  slug = paste0("id-test-", new_oid()),
  name = "inspect"
))
got <- mongo_one("organizations", q_field("slug", doc$slug))
cat("class=", paste(class(got[["_id"]]), collapse = ","), "\n", sep = "")
cat("typeof=", typeof(got[["_id"]]), "\n", sep = "")
str(got[["_id"]])
cat("names=", paste(names(got), collapse = ","), "\n", sep = "")
mongo_col("organizations")$remove(q_field("slug", doc$slug))
