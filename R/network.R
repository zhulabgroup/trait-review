# ls_df_para <- get_keyphrase_paragraph()

get_keyphrase_paragraph <- function(df_bib_phrase = NULL, df_phrase_stem = NULL, df_phrase_label = NULL, indir = "inst/extdata/keywords/", outdir = "inst/extdata/network/") {
  if (is.null(df_bib_phrase)) {
    df_bib_phrase <- read_csv(str_c(indir, "df_bib_phrase.csv"))
  }
  if (is.null(df_phrase_stem)) {
    df_phrase_stem <- read_csv(str_c(indir, "df_phrase_stem.csv"))
  }
  if (is.null(df_phrase_label)) {
    df_phrase_label <- read_csv(str_c(indir, "df_phrase_label_done.csv"))
  }
  df_phrase_label <- df_phrase_label %>%
    mutate(valid = replace_na(valid, 1)) %>%
    filter(valid == 1) %>%
    mutate(
      trait = replace_na(trait, 0),
      globalchange = replace_na(globalchange, 0)
    )


  df_bib_phrase_sub <- df_bib_phrase %>%
    right_join(df_phrase_stem %>% select(-keyphrase_n), by = "keyphrase") %>%
    right_join(df_phrase_label %>% select(-count), by = "keyphrase_stem")

  df_net_area_trait <- df_bib_phrase_sub %>%
    filter(trait == 1) %>%
    select(group = code, phrase = keyphrase_eg) %>%
    group_by(group, phrase) %>%
    summarise(count = n()) %>%
    ungroup()
  write_csv(df_net_area_trait, str_c(outdir, "df_net_area_trait.csv"))

  df_net_trait_all <- df_bib_phrase_sub %>%
    filter(trait != 1) %>%
    select(id, code, phrase = keyphrase_eg) %>%
    group_by(id, code, phrase) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    inner_join(
      df_bib_phrase_sub %>%
        filter(trait == 1) %>%
        distinct(id, code, group = keyphrase_eg),
      by = c("id", "code")
    ) %>%
    gather(key = "phrase", value = "count", -id, -code, -group) %>%
    select(-id, -code) %>%
    group_by(group, phrase) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    filter(count > 0)
  write_csv(df_net_trait_all, str_c(outdir, "df_net_trait_all.csv"))

  df_net_trait_gc <- df_bib_phrase_sub %>%
    filter(globalchange == 1) %>%
    select(id, code, phrase = keyphrase_eg) %>%
    group_by(id, code, phrase) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    inner_join(
      df_bib_phrase_sub %>%
        filter(trait == 1) %>%
        distinct(id, code, group = keyphrase_eg),
      by = c("id", "code")
    ) %>%
    gather(key = "phrase", value = "count", -id, -code, -group) %>%
    select(-id, -code) %>%
    group_by(group, phrase) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    filter(count > 0)
  write_csv(df_net_trait_gc, str_c(outdir, "df_net_trait_gc.csv"))

  out <- list(
    area_trait = df_net_area_trait,
    trait_all = df_net_trait_all,
    trait_gc = df_net_trait_gc
  )
  return(out)
}
