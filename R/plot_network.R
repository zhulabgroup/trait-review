# plot_text_network(ls_df_para$area_trait)
# plot_text_network(ls_df_para$trait_all %>% filter(count >=100))
# plot_text_network(ls_df_para$trait_gc)

plot_text_network <- function(df_net, min_count = 0, label = T, save = F, filename = NULL) {
  # generate network
  bip <- df_net %>%
    select(group, phrase, count) %>%
    filter(count >= min_count) %>%
    # mutate(count  = if_else(count <=0, 0, count)) %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    column_to_rownames("group") %>%
    as.matrix()

  network <- ggbipart::bip_init_network(bip)

  # set attributes
  df_node <- data.frame(group = network %>% network.vertex.names()) %>%
    mutate(mode = case_when(
      group %in% (df_net$group %>% unique()) ~ "group",
      TRUE ~ "phrase"
    ))

  size_pal <- c(2, 0.1)
  names(size_pal) <- c("group", "phrase")
  network %v% "size" <- size_pal[df_node$mode]

  alpha_pal <- c(1, 0.25)
  names(alpha_pal) <- c("group", "phrase")
  network %v% "alpha" <- alpha_pal[df_node$mode]

  if (label) {
    ls_group <- df_net %>%
      distinct(group) %>%
      pull(group)
    ls_freq_phrase <- df_net %>%
      group_by(phrase) %>%
      summarise(count = sum(count)) %>%
      arrange(desc(count)) %>%
      head(3) %>%
      pull(phrase)
    set.seed(12)
    ls_rand_phrase <- df_net %>%
      group_by(group) %>%
      sample_n(1) %>%
      ungroup() %>%
      pull(phrase) %>%
      unique()

    network %v% "label" <- df_node %>%
      mutate(label = case_when(group %in% ls_group |
        group %in% ls_freq_phrase |
        group %in% ls_rand_phrase
      ~ group)) %>%
      pull(label)
  } else {
    network %v% "label" <- ""
  }

  edge_size <- ggbipart::bip_edgewt(bip, x = 2)
  edge_alpha <- edge_size / 50
  # edge_alpha[edge_alpha < quantile(edge_alpha, 0.75)] <- 0

  if ("supergroup" %in% colnames(df_net)) {
    df_node <- df_node %>%
      left_join(df_net %>% distinct(group, supergroup), by = "group") %>%
      mutate(supergroup = replace_na(supergroup, "phrase"))
    network %v% "color" <- df_node$supergroup
    col_pal <- c(RColorBrewer::brewer.pal(df_net %>% pull(supergroup) %>% unique() %>% length(), "Set2"), "light gray")
    names(col_pal) <- c(df_net %>% pull(supergroup) %>% unique(), "phrase")
  } else {
    network %v% "color" <- df_node$mode
    col_pal <- c("orange", "light gray")
    names(col_pal) <- c("group", "phrase")
  }

  set.seed(12)
  p <- GGally::ggnet2(
    net = network,
    shape = "mode",
    label = "label",
    color = "color",
    palette = col_pal,
    size = "size",
    legend.size = 9,
    node.alpha = "alpha",
    label.alpha = "alpha",
    mode = "fruchtermanreingold",
    # mode = "spring",
    label.size = 3,
    layout.par = NULL,
    layout.exp = 0.25,
    label.trim = FALSE,
    edge.lty = "solid",
    edge.label = NULL,
    edge.size = edge_size,
    edge.alpha = edge_alpha
  ) +
    guides(
      size = "none",
      pch = "none",
      color = guide_legend(label.position = "left", title = "")
    )


  if (save) {
    if (!is.null(filename)) {
      ggsave(str_c("inst/figures/", filename, ".png"), p,
        width = 16,
        height = 8
      )
      ggsave(str_c("alldata/output/figures/", filename, ".png"), p,
        width = 16,
        height = 8
      )
    }
  }
  return(p)
}
