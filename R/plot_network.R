# plot_text_network(df_net_area_trait)
# plot_text_network(df_net_trait_all)
# plot_text_network(df_net_trait_gc)

plot_text_network <- function(df_net) {
  bip <- df_net %>%
    # mutate(count  = if_else(count <=0, 0, count)) %>%
    spread(key = "phrase", value = "count", fill = 0) %>%
    column_to_rownames("group") %>%
    as.matrix()

  network <- ggbipart::bip_init_network(bip)
  edge_size <- ggbipart::bip_edgewt(bip, x = 2)
  edge_alpha <- edge_size / 10
  edge_alpha[edge_alpha < quantile(edge_alpha, 0.75)] <- 0

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
    layout.par = NULL,
    layout.exp = 0.25,
    size.legend = NA,
    label.trim = FALSE,
    edge.lty = "solid",
    edge.label = NULL,
    edge.size = edge_size,
    edge.alpha = edge_alpha
  ) +
    theme(legend.position = "none")
  return(p)
}
