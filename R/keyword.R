# df_bib_phrase <- get_all_keyphrase(df_bib)
# df_phrase_stem <- get_keyphrase_stem(df_bib_phrase)

get_keyphrase_stem <- function(df_bib_phrase = NULL, indir = "inst/extdata/keywords/", outdir = "inst/extdata/keywords/") {
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
    filter(count >= n_paper * 0.005)

  df_phrase_label <- df_phrase_stem %>%
    mutate(
      valid = "",
      trait = "",
      globalchange = ""
    )

  write_csv(df_phrase_stem, str_c(outdir, "df_phrase_stem.csv"))
  write_csv(df_phrase_label, str_c(outdir, "df_phrase_label.csv"))
  return(df_phrase_stem)
}

get_all_keyphrase <- function(df_bib, outdir = "inst/extdata/keywords/") {
  df_bib_text <- df_bib %>%
    select(code, title = TITLE, abstract = ABSTRACT, keyword = AUTHOR_KEYWORDS) %>%
    mutate(full = str_c(title, " ", abstract)) %>%
    select(code, full, keyword) %>%
    mutate(code = if_else(str_detect(code, "ENVI"), "ENVI", code)) %>%
    mutate(id = row_number())

  # df_bib_text<- df_bib_text %>% sample_frac(0.05)

  n_paper <- nrow(df_bib_text)
  # df_code_summ <- df_bib_text %>%
  #   group_by(code) %>%
  #   summarise(n = n()) %>%
  #   arrange(desc(n))

  cl <- makeCluster(30, outfile = "")
  registerDoSNOW(cl)

  ls_df_bib_phrase <-
    foreach(
      i = 1:nrow(df_bib_text),
      .packages = c("tidyverse", "litsearchr")
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
  stopcluster(cl)

  df_bib_phrase <- bind_rows(ls_df_bib_phrase)

  write_csv(df_bib_phrase, str_c(outdir, "df_bib_phrase.csv"))

  return(df_bib_phrase)
}

extract_keyphrase <- function(full_text, keyword_author) {
  v_keyword_fakerake <- litsearchr::extract_terms(text = full_text, method = "fakerake", min_freq = 1, min_n = 2, ngrams = T)
  v_keyword_author <- keyword_author %>%
    str_split(";", simplify = T) %>%
    as.character() %>%
    tolower() %>%
    trimws()
  v_keyword <- union(v_keyword_fakerake, v_keyword_author) %>% sort()
  # df_keyword <- data.frame(keyword = v_keyword)

  return(v_keyword)
}
