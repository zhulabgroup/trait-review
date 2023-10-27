# df_bib <- read_bib(path = "alldata/bib/20230918")

read_bib <- function(path = "alldata/bib/20230918", num_core = 35) {
  v_file_bib <- list.files(path, full.names = T)

  cl <- makeCluster(num_core, outfile = "")
  registerDoSNOW(cl)

  ls_df_bib <-
    foreach(
      f = v_file_bib,
      .packages = c("tidyverse")
    ) %dopar% {
      area_code <- f %>%
        strsplit("/") %>%
        unlist() %>%
        tail(1) %>%
        str_remove("scopus_") %>%
        str_remove(".csv")
      area <- area_code %>%
        strsplit("_") %>%
        unlist() %>%
        head(1)
      code <- area_code %>%
        strsplit("_") %>%
        unlist() %>%
        tail(1)
      print(area_code)

      read_csv(f) %>%
        mutate(keyword = str_c(`Author Keywords`, `Index Keywords`, sep = "; ")) %>%
        select(year = Year, title = Title, abstract = Abstract, keyword) %>%
        mutate(
          area = area,
          code = code
        )
    }
  df_bib <- bind_rows(ls_df_bib) %>%
    # janitor::remove_empty(which = "cols") %>%
    select(area, code, everything())

  usethis::use_data(df_bib, overwrite = T)

  return(df_bib)
}

get_bib_code <- function(df_bib, save = T) {
  df_bib_code <- df_bib %>%
    mutate(id = row_number()) %>%
    select(id, area, code)

  if (save) {
    usethis::use_data(df_bib_code, overwrite = T)
  }
  return(df_bib_code)
}
