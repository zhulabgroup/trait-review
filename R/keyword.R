# df_bib_phrase <- get_all_keyphrase(df_bib)
# df_phrase_stem <- get_keyphrase_stem(df_bib_phrase)

get_all_keyphrase <- function(df_bib, outdir = "alldata/intermediate/keyword/", num_core= 35) {
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
          id = i,
          code = df_bib_text$code[i]
        )
      print(str_c(i, " out of ", n_paper))
      df_bib_phrase_i
    }
  stopCluster(cl)

  df_bib_phrase <- bind_rows(ls_df_bib_phrase)

  write_csv(df_bib_phrase, str_c(outdir, "df_bib_phrase.csv"))

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

get_keyphrase_stem <- function(df_bib_phrase = NULL, indir = "alldata/intermediate/keyword/", outdir = "alldata/intermediate/keyword/") {
  if (is.null(df_bib_phrase)) {
    df_bib_phrase <- read_csv(str_c(indir, "df_bib_phrase.csv"))
  }
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

  write_csv(df_phrase_stem, str_c(outdir, "df_phrase_stem.csv"))
  write_csv(df_phrase_label, str_c(outdir, "df_phrase_label.csv"))
  return(df_phrase_stem)
}


