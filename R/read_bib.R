# df_bib <- read_bib()

read_bib <- function() {
  path <- system.file("extdata/bib/", package = "traitlitreview")
  v_file_bib <- list.files(path, full.names = T)

  ls_df_bib <- vector(mode = "list")
  for (f in v_file_bib) {
    code <- f %>%
      strsplit("//") %>%
      unlist() %>%
      `[`(2) %>%
      str_remove("scopus_") %>%
      str_remove(".bib")
    ls_df_bib[[code]] <- bib2df::bib2df(f) %>%
      mutate(code = code)
    print(code)
  }
  df_bib <- bind_rows(ls_df_bib) %>%
    janitor::remove_empty(which = "cols") %>%
    select(code, everything())

  usethis::use_data(df_bib)
}
