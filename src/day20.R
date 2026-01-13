library("collections", include.only = "queue")
library("gmp", include.only = c("as.bigz", "lcm.bigz"))

make_pulse <- function(node, level, from) {
  list(node = node, level = level, from = from)
}

build_graph <- function(data) {
  graph <- igraph::make_empty_graph(n = length(data), directed = TRUE)
  parts <- strsplit(data, " -> ")
  nodes <- sapply(parts, function(x) x[1])
  node_types <- sapply(nodes, function(x) {
    if (grepl("^broadcaster", x)) {
      "broadcaster"
    } else {
      substr(x, 1, 1)
    }
  })
  node_labels <- sapply(nodes, function(x) {
    if (grepl("^broadcaster", x)) {
      "broadcaster"
    } else {
      substr(x, 2, nchar(x))
    }
  })
  for (i in seq_along(parts)) {
    graph <- igraph::set_vertex_attr(graph, "type", i, node_types[i])
    graph <- igraph::set_vertex_attr(graph, "label", i, node_labels[i])
    if (node_types[i] == "%") {
      graph <- igraph::set_vertex_attr(graph, "state", i, FALSE)
    } else if (node_types[i] == "&") {
      # A bitset, each bit corresponds to one input
      graph <- igraph::set_vertex_attr(graph, "state", i, 0)
    }
    targets <- strsplit(parts[[i]][2], ", ")[[1]]
    for (target in targets) {
      if (!(target %in% node_labels)) {
        node_labels <- c(node_labels, target)
        graph <- igraph::add_vertices(graph, 1, type = "", label = target)
      }
      graph <- igraph::add_edges(graph, c(i, match(target, node_labels)))
    }
  }
  graph
}

push_btn <- function(graph, broadcaster, track_inputs = NULL) {
  nbrs <- igraph::neighbors(graph, broadcaster, "out")
  q <-
    queue(items = lapply(nbrs, function(x) make_pulse(x, FALSE, broadcaster)))
  # Include 1 from the button to the broadcaster
  low_pulses <- length(nbrs) + 1
  high_pulses <- 0
  inputs <- list()
  while (q$size() > 0) {
    pulse <- q$pop()
    node_type <- igraph::vertex_attr(graph, "type", pulse$node)
    out_level <- NULL
    if (node_type == "%") {
      if (pulse$level == TRUE) {
        next
      }
      cur_st <- igraph::vertex_attr(graph, "state", pulse$node)
      cur_st <- !cur_st
      graph <- igraph::set_vertex_attr(graph, "state", pulse$node, cur_st)
      out_level <- cur_st
    } else if (node_type == "&") {
      cur_st <- igraph::vertex_attr(graph, "state", pulse$node)
      node_inputs <- igraph::neighbors(graph, pulse$node, "in")
      from_ind <- match(pulse$from, node_inputs)
      if (pulse$level == TRUE) {
        cur_st <- bitwOr(cur_st, bitwShiftL(1L, from_ind - 1))
      } else {
        cur_st <- bitwAnd(cur_st, bitwNot(bitwShiftL(1L, from_ind - 1)))
      }
      graph <- igraph::set_vertex_attr(graph, "state", pulse$node, cur_st)
      out_level <- cur_st != bitwShiftL(1L, length(node_inputs)) - 1
    }
    if (!is.null(out_level)) {
      nbrs <- igraph::neighbors(graph, pulse$node, "out")
      for (nbr in nbrs) {
        new_pulse <- make_pulse(nbr, out_level, pulse$node)
        q$push(new_pulse)
        if (!is.null(track_inputs) && track_inputs == nbr) {
          key <- igraph::vertex_attr(graph, "label", pulse$node)
          if (!(key %in% names(inputs))) {
            inputs[[key]] <- c()
          }
          inputs[[key]] <- c(inputs[[key]], out_level)
        }
      }
      if (out_level) {
        high_pulses <- high_pulses + length(nbrs)
      } else {
        low_pulses <- low_pulses + length(nbrs)
      }
    }
  }
  list(graph = graph, pulses = c(low_pulses, high_pulses), inputs = inputs)
}

solve1 <- function(data) {
  graph <- build_graph(data)
  broadcaster <- which(igraph::vertex_attr(graph, "label") == "broadcaster")
  dgr <- DiagrammeR::from_igraph(graph)
  dgr <- DiagrammeR::set_node_attrs(dgr, "width", 0.25)
  dgr <- DiagrammeR::set_node_attrs(dgr, "height", 0.25)
  dgr <- DiagrammeR::set_node_attrs(
    dgr,
    "color",
    c("black", "blue", "green", "red")[
      match(igraph::vertex_attr(graph, "type"), c("broadcaster", "%", "&", ""))
    ]
  )
  DiagrammeR::export_graph(dgr, file_name = "src/day20.png")

  pulses <- c(as.bigz(0), as.bigz(0))
  for (i in 1:1000) {
    result <- push_btn(graph, broadcaster)
    graph <- result$graph
    pulses <- pulses + as.bigz(result$pulses)
  }
  cat(as.character(pulses[1] * pulses[2]), "\n")
}

# Key observation from day20.png: rx comes a single conjunction of four other
# conjunctions. We need each conjunction to send a single high pulse
solve2 <- function(data) {
  graph <- build_graph(data)
  broadcaster <- which(igraph::vertex_attr(graph, "label") == "broadcaster")
  rx_parent <- igraph::neighbors(
    graph,
    which(igraph::vertex_attr(graph, "label") == "rx"),
    "in"
  )
  if (length(rx_parent) != 1) {
    stop("Expected exactly one parent of rx")
  }
  rx_grandparents <- igraph::neighbors(graph, rx_parent, "in")
  first_time_single_high <- list()
  i <- 1
  repeat {
    result <- push_btn(graph, broadcaster, rx_parent)
    graph <- result$graph
    inputs <- result$inputs
    for (p in rx_grandparents) {
      key <- igraph::vertex_attr(graph, "label", p)
      if (sum(inputs[[key]]) == 1) {
        if (!(key %in% names(first_time_single_high))) {
          first_time_single_high[[key]] <- as.bigz(i)
        }
      }
    }
    if (length(first_time_single_high) == length(rx_grandparents)) {
      break
    }
    i <- i + 1
  }
  cat(as.character(Reduce(lcm.bigz, first_time_single_high)), "\n")
}
