# plot_text_network(ls_df_para$area_trait)
# plot_text_network(ls_df_para$trait_all %>% filter(count >=100))
# plot_text_network(ls_df_para$trait_gc)

plot_text_network <- function(df_net, min_count = 0, save = F, filename = NULL) {

  bip <- df_net %>%
    select(group, phrase, count) %>%
    filter(count >= min_count) %>%
    # mutate(count  = if_else(count <=0, 0, count)) %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    column_to_rownames("group") %>%
    as.matrix()

  network <- ggbipart::bip_init_network(bip)
  edge_size <- ggbipart::bip_edgewt(bip, x = 2)
  edge_alpha <- edge_size / 10
  # edge_alpha[edge_alpha < quantile(edge_alpha, 0.75)] <- 0

  if ("big_group" %in% colnames(df_net)) {

    df_node <- data.frame(group = network %>% network.vertex.names()) %>%
      left_join(df_net %>% distinct(group, big_group), by = "group")
    network %v% "color" <- df_node$big_group
    col_pal <- c(RColorBrewer::brewer.pal(df_net %>% pull(big_group) %>% unique() %>% length(), "Set2"), "gray")
    names(col_pal) = c(df_net %>% pull(big_group) %>% unique(), NA)

    set.seed(12)
    p <- GGally::ggnet2(
      net = network,
      shape = "mode",
      label = T,
      color = "color",
      palette = col_pal,
      size = 9,
      legend.size = 9,
      mode = "fruchtermanreingold",
      # mode = "spring",
      label.size = 4,
      label.alpha = 0.5,
      layout.par = NULL,
      layout.exp = 0.25,
      size.legend = NA,
      label.trim = FALSE,
      edge.lty = "solid",
      edge.label = NULL,
      edge.size = edge_size,
      edge.alpha = edge_alpha,
      node.alpha = 0.5
    ) +
      theme(legend.position = "none")

  } else {
    set.seed(12)
    p <- GGally::ggnet2(
      net = network,
      shape = "mode",
      label = T,
      color = "mode",
      palette = c(A = "grey", P = "gold"),
      size = 9,
      legend.size = 9,
      mode = "fruchtermanreingold",
      # mode = "spring",
      label.size = 4,
      label.alpha = 0.5,
      layout.par = NULL,
      layout.exp = 0.25,
      size.legend = NA,
      label.trim = FALSE,
      edge.lty = "solid",
      edge.label = NULL,
      edge.size = edge_size,
      edge.alpha = edge_alpha,
      node.alpha = 0.5
    ) +
      theme(legend.position = "none")

  }

  if (save) {
    if (!is.null(filename)) {
      ggsave(str_c("inst/figures/", filename, ".png"), p)
    }
  }
  return(p)
}
