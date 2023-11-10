# df_bib_phrase <- get_all_keyphrase(df_bib)
# df_phrase_stem <- get_keyphrase_stem(df_bib_phrase)
# df_phrase_label <- read_keyphrase_label()

get_all_keyphrase <- function(df_bib, num_core = 11, save = T) {
  df_bib_text <- df_bib %>%
    select(area, code, title, abstract, keyword) %>%
    mutate(full = str_c(title, " ", abstract)) %>%
    select(area, code, full, keyword) %>%
    mutate(id = row_number())

  # df_bib_text<- df_bib_text %>% sample_frac(0.05)

  n_paper <- nrow(df_bib_text)
  # df_code_summ <- df_bib_text %>%
  #   group_by(code) %>%
  #   summarise(n = n()) %>%
  #   arrange(desc(n))

  cl <- makeCluster(num_core, outfile = "")
  registerDoSNOW(cl)

  ls_df_bib_phrase <-
    foreach(
      i = 1:nrow(df_bib_text),
      .packages = c("tidyverse", "litsearchr", "traitlitreview")
    ) %dopar% {
      df_bib_phrase_i <- data.frame(
        keyphrase = extract_keyphrase(df_bib_text$full[i], df_bib_text$keyword[i])
      ) %>%
        mutate(
          id = i # ,
          # code = df_bib_text$code[i]
        )
      print(str_c(i, " out of ", n_paper))
      df_bib_phrase_i
    }
  stopCluster(cl)

  df_bib_phrase <- bind_rows(ls_df_bib_phrase)

  # df_phrase_id <- df_bib_phrase %>%
  #   distinct(keyphrase) %>%
  #   arrange(keyphrase) %>%
  #   mutate(keyphrase_id = row_number())
  #
  # df_bib_phrase_id <- df_bib_phrase %>%
  #   left_join(df_phrase_id, by = "keyphrase") %>%
  #   select(-keyphrase)

  if (save) {
    usethis::use_data(df_bib_phrase, overwrite = T)
  }

  return(df_bib_phrase)
}

extract_keyphrase <- function(full_text, keyword_original) {
  v_keyword_fakerake <- litsearchr::extract_terms(text = full_text, method = "fakerake", min_freq = 1, min_n = 2, ngrams = T)
  v_keyword_original <- keyword_original %>%
    str_split(";", simplify = T) %>%
    as.character() %>%
    tolower() %>%
    trimws()
  v_keyword <- union(v_keyword_fakerake, v_keyword_original) %>% sort()
  # df_keyword <- data.frame(keyword = v_keyword)

  return(v_keyword)
}

get_keyphrase_stem <- function(df_bib_phrase, outdir = "alldata/intermediate/keyword/20231109/", save = T) {
  n_paper <- df_bib_phrase %>%
    pull(id) %>%
    unique() %>%
    length()

  df_phrase_stem <- df_bib_phrase %>%
    group_by(keyphrase) %>%
    summarise(keyphrase_n = id %>% unique() %>% length()) %>%
    filter(keyphrase_n >= 50) %>%
    ungroup() %>%
    arrange(desc(keyphrase_n)) %>%
    mutate(keyphrase_stem = tm::stemDocument(keyphrase))

  df_phrase_label <- df_phrase_stem %>%
    group_by(keyphrase_stem) %>%
    arrange(desc(keyphrase_n)) %>%
    summarise(
      count = sum(keyphrase_n),
      keyphrase_eg = keyphrase[1]
    ) %>%
    arrange(desc(count)) %>%
    select(keyphrase_stem, keyphrase_eg, count) %>%
    filter(count >= n_paper * 0.002) %>%
    mutate(
      valid = "",
      trait = "",
      globalchange = ""
    )

  if (save) {
    usethis::use_data(df_phrase_stem, overwrite = T)
    usethis::use_data(df_phrase_label, overwrite = T)
  }

  write_csv(df_phrase_label, str_c(outdir, "df_phrase_label.csv"))
  return(df_phrase_stem)
}

read_keyphrase_label <- function(file = "alldata/intermediate/keyword/df_phrase_label_20231025.csv", save = T) {
  df_phrase_label <- read_csv(file) %>%
    mutate(
      trait_inclusive = str_replace(trait_inclusive, "\\?", "1"),
      trait_exclusive = str_replace(trait_exclusive, "\\?", "1"),
      globalchange = str_replace(globalchange, "\\?", "1")
    ) %>%
    mutate(
      trait_inclusive = trait_inclusive %>% as.numeric(),
      trait_exclusive = trait_exclusive %>% as.numeric(),
      globalchange = globalchange %>% as.numeric()
    ) %>%
    mutate(valid = replace_na(valid, 1)) %>%
    filter(valid == 1) %>%
    mutate(
      trait_inclusive = replace_na(trait_inclusive, 0),
      trait_exclusive = replace_na(trait_exclusive, 0),
      globalchange = replace_na(globalchange, 0)
    )
  if (save) {
    usethis::use_data(df_phrase_label, overwrite = T)
  }
  return(df_phrase_label)
}
