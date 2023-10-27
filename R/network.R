# ls_df_net <- make_network(df_bib_phrase, df_phrase_stem, df_phrase_label)

make_network <- function(df_bib_phrase, df_phrase_stem, df_phrase_label, df_bib_code, save = T) {
  df_bib_phrase_sub <- df_bib_phrase %>%
    right_join(df_phrase_stem %>% select(-keyphrase_n), by = "keyphrase") %>%
    right_join(df_phrase_label %>% select(keyphrase_stem, keyphrase_eg, valid, trait_inclusive, trait_exclusive, globalchange), by = "keyphrase_stem") %>%
    left_join(df_bib_code, by = "id")

  df_net_area_trait <- df_bib_phrase_sub %>%
    filter(trait_inclusive == 1) %>%
    select(supergroup = area, group = code, phrase = keyphrase_eg) %>%
    group_by(supergroup, group, phrase) %>%
    summarise(count = n()) %>%
    ungroup()

  df_net_trait_all <- df_bib_phrase_sub %>%
    filter(trait_inclusive != 1) %>%
    select(id, phrase = keyphrase_eg) %>%
    group_by(id, phrase) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    inner_join(
      df_bib_phrase_sub %>%
        filter(trait_inclusive == 1) %>%
        distinct(id, group = keyphrase_eg),
      by = c("id")
    ) %>%
    gather(key = "phrase", value = "count", -id, -group) %>%
    select(-id) %>%
    group_by(group, phrase) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    filter(count > 0)

  df_net_trait_gc <- df_bib_phrase_sub %>%
    filter(globalchange == 1) %>%
    select(id, phrase = keyphrase_eg) %>%
    group_by(id, phrase) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    inner_join(
      df_bib_phrase_sub %>%
        filter(trait_exclusive == 1) %>%
        distinct(id, group = keyphrase_eg),
      by = c("id")
    ) %>%
    gather(key = "phrase", value = "count", -id, -group) %>%
    select(-id) %>%
    group_by(group, phrase) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>%
    filter(count > 0)

  df_net_area_gc <- df_bib_phrase_sub %>%
    filter(globalchange == 1) %>%
    select(supergroup = area, group = code, phrase = keyphrase_eg) %>%
    group_by(supergroup, group, phrase) %>%
    summarise(count = n()) %>%
    ungroup()

  ls_df_net <- list(
    area_trait = df_net_area_trait,
    trait_all = df_net_trait_all,
    trait_gc = df_net_trait_gc,
    area_gc = df_net_area_gc
  )

  if (save) {
    usethis::use_data(ls_df_net, overwrite = T)
  }
  return(ls_df_net)
}
