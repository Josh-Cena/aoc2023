solve1 <- function(data) {
  parts <- strsplit(data, ": ")
  nodes <- sapply(parts, function(x) x[1])
  targets <- lapply(parts, function(x) strsplit(x[2], " ")[[1]])
  nodes <- unique(c(nodes, unlist(targets)))
  graph <- igraph::make_empty_graph(n = length(nodes), directed = FALSE)
  for (i in seq_along(targets)) {
    graph <- igraph::add_edges(graph,
      unlist(sapply(targets[[i]], function(x) c(i, match(x, nodes)))))
  }
  igraph::V(graph)$label <- nodes
  dgr <- DiagrammeR::from_igraph(graph)
  dgr <- DiagrammeR::set_node_attrs(dgr, "width", 0.25)
  dgr <- DiagrammeR::set_node_attrs(dgr, "height", 0.25)
  DiagrammeR::export_graph(dgr, file_name = "src/day25.png")
  min_cut <- igraph::min_cut(graph, value.only = FALSE)
  stopifnot(min_cut$value == 3)
  cat(length(min_cut$partition1) * length(min_cut$partition2), "\n")
}

solve2 <- function(data) {
  cat("No such thing, yay\n")
}
