library("DiagrammeR")

solve1 <- function(data) {
  parts <- strsplit(data, ": ")
  nodes <- sapply(parts, function(x) x[1])
  targets <- lapply(parts, function(x) strsplit(x[2], " ")[[1]])
  edges_from <- unlist(lapply(seq_along(targets), function(i) {
    rep(nodes[[i]], length(targets[[i]]))
  }))
  targets <- unlist(targets)
  nodes <- unique(c(nodes, targets))
  edges_from <- match(edges_from, nodes)
  targets <- match(targets, nodes)
  nodes <- create_node_df(
    n = length(nodes),
    label = nodes,
    width = 0.25,
    height = 0.25
  )
  edges <- create_edge_df(
    from = edges_from,
    to = targets
  )
  graph <- create_graph(nodes_df = nodes, edges_df = edges)
  graph <- set_graph_undirected(graph)
  export_graph(graph, file_name = "src/day25.png")

  # For example
  # edges_to_cut <- list(
  #   c("hfx", "pzl"),
  #   c("bvb", "cmg"),
  #   c("nvd", "jqt")
  # )

  edges_to_cut <- list(
    c("ffj", "lkm"),
    c("ljl", "xhg"),
    c("vgs", "xjb")
  )

  for (edge in edges_to_cut) {
    graph <- delete_edge(graph, from = match(edge[1], nodes$label),
                to = match(edge[2], nodes$label))
  }

  comp1size <- length(get_all_connected_nodes(graph,
                  match(edges_to_cut[[1]][1], nodes$label))) + 1
  comp2size <- length(get_all_connected_nodes(graph,
                  match(edges_to_cut[[1]][2], nodes$label))) + 1
  cat(comp1size * comp2size, "\n")
  export_graph(graph, file_name = "src/day25_cut.png")
}

solve2 <- function(data) {
  cat("No such thing, yay\n")
}
