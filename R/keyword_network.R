library(doSNOW)
library(parallel)

get_all_keyphrase <- function(df_bib) {
  df_bib_text <- df_bib %>%
    select(code, title = TITLE, abstract = ABSTRACT, keyword = AUTHOR_KEYWORDS) %>%
    mutate(full = str_c(title, " ", abstract)) %>%
    select(code, full, keyword) %>%
    mutate(code = if_else(str_detect(code, "ENVI"), "ENVI", code)) %>%
    mutate(id = row_number())

  # df_bib_text<- df_bib_text %>% sample_frac(0.05)

  n_paper <- nrow(df_bib_text)
  df_code_summ <- df_bib_text %>%
    group_by(code) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

  cl <- makeCluster(30, outfile = "")
  registerDoSNOW(cl)

  ls_df_bib_phrase<-
    foreach (i = 1:nrow(df_bib_text),
           .packages = c("tidyverse", "litsearchr")) %dopar% {
    df_bib_phrase_i<-data.frame(
               keyphrase =extract_keyphrase(df_bib_text$full[i], df_bib_text$keyword[i])
    ) %>%
      mutate(id = i,
             code = df_bib_text$code[i])
    print( str_c(i, " out of ",n_paper ))
    df_bib_phrase_i
           }
  stopcluster(cl)

  df_bib_phrase <- bind_rows(ls_df_bib_phrase)

  df_phrase_freq <- df_bib_phrase %>%
    group_by(keyphrase) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    filter(count >= n_paper*0.005)
  df_bib_phrase_freq <- df_bib_phrase %>%
    right_join(df_phrase_freq %>% select(keyphrase), by = "keyphrase")

  df_bib_phrase_para <- df_bib_phrase_freq %>%
    group_by(id, code) %>%
    nest() %>%
    mutate(text = data [[1]]%>%
             pull(keyphrase) %>%
               str_replace_all("-", "_") %>%
               str_replace_all(" ", "__") %>%
               paste( collapse = ' ')
           ) %>%
    select(id, code, text)

  # df_bib_phrase_summ <- df_bib_phrase %>%

  # usethis::use_data(df_bib_phrase)

  write_csv(df_bib_phrase, "data/df_phrase.csv")
  write_csv(df_bib_phrase_para, "data/df_phrase_para.csv")

}

extract_keyphrase <- function(full_text, keyword_author) {
  v_keyword_fakerake <- litsearchr::extract_terms(text=full_text, method="fakerake", min_freq=1, min_n=2,  ngrams = T)
  v_keyword_author <- keyword_author %>% str_split(";", simplify = T) %>% as.character()%>% tolower() %>% trimws()
  v_keyword <- union(v_keyword_fakerake, v_keyword_author) %>% sort()
  # df_keyword <- data.frame(keyword = v_keyword)

  return(v_keyword)
}
