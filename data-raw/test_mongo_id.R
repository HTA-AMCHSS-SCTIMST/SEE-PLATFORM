readRenviron(".Renviron")
for (f in c("config.R", "log.R", "ids.R", "mongo.R")) source(file.path("R", f))
doc <- mongo_insert("organizations", list(
  name = "id-type-test",
  slug = paste0("id-test-", new_oid()),
  settings = list(allowExpertSelfSignup = FALSE, defaultProtocol = "shelf"),
  createdAt = iso_now()
))
cat("inserted _id=", doc_id(doc), "\n", sep = "")
got <- mongo_one("organizations", q_id(doc_id(doc)))
cat("roundtrip _id=", doc_id(got), " slug=", got$slug, "\n", sep = "")
mongo_col("organizations")$remove(q_id(doc_id(doc)))
cat("ok\n")
