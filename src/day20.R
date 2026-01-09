library("DiagrammeR")
library("collections", include.only = "queue")
library("gmp", include.only = c("as.bigz", "lcm.bigz"))

make_pulse <- function(node, level, from) {
  list(node = node, level = level, from = from)
}

build_graph <- function(data) {
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
  targets <- lapply(parts, function(x) strsplit(x[2], ", ")[[1]])
  for (target in unlist(targets)) {
    if (!(target %in% node_labels)) {
      node_labels <- c(node_labels, target)
      node_types <- c(node_types, "")
    }
  }
  edges_from <- unlist(lapply(seq_along(targets), function(i) {
    rep(node_labels[[i]], length(targets[[i]]))
  }))
  targets <- unlist(targets)
  edges_from <- match(edges_from, node_labels)
  targets <- match(targets, node_labels)
  nodes <- create_node_df(
    n = length(node_labels),
    label = node_labels,
    type = node_types,
    color = c("black", "blue", "green", "red")[
      match(node_types, c("broadcaster", "%", "&", ""))
    ],
    width = 0.25,
    height = 0.25
  )
  edges <- create_edge_df(
    from = edges_from,
    to = targets
  )
  graph <- create_graph(nodes_df = nodes, edges_df = edges)
  flip_flop_states <- rep(FALSE, length(node_labels))
  conjunction_states <- list()
  for (i in seq_along(node_labels)) {
    if (node_types[i] == "&") {
      conjunction_states[[i]] <- list()
      for (nbr in get_predecessors(graph, i)) {
        conjunction_states[[i]][[nbr]] <- "low"
      }
    }
  }
  return(list(
    graph = graph,
    flip_flop_states = flip_flop_states,
    conjunction_states = conjunction_states
  ))
}

push_btn <- function(
  graph,
  flip_flop_states,
  conjunction_states,
  track_inputs = NULL
) {
  nodes <- get_node_df(graph)
  broadcaster <- nodes[nodes$label == "broadcaster", "id"]
  nbrs <- get_successors(graph, broadcaster)
  q <-
    queue(items = lapply(nbrs, function(x) make_pulse(x, "low", broadcaster)))
  # Include 1 from the button to the broadcaster
  low_pulses <- length(nbrs) + 1
  high_pulses <- 0
  inputs <- list()
  while (q$size() > 0) {
    pulse <- q$pop()
    node_type <- nodes[pulse$node, "type"]
    new_level <- ""
    if (node_type == "%") {
      if (pulse$level == "high") {
        next
      }
      flip_flop_states[pulse$node] <- !flip_flop_states[pulse$node]
      new_level <- if (flip_flop_states[pulse$node]) "high" else "low"
    } else if (node_type == "&") {
      conjunction_states[[pulse$node]][[pulse$from]] <- pulse$level
      if (all(unlist(conjunction_states[[pulse$node]]) == "high")) {
        new_level <- "low"
      } else {
        new_level <- "high"
      }
    }
    if (new_level != "") {
      nbrs <- get_successors(graph, pulse$node)
      for (nbr in nbrs) {
        new_pulse <- make_pulse(nbr, new_level, pulse$node)
        q$push(new_pulse)
        if (nbr %in% track_inputs) {
          key <- as.character(pulse$node)
          if (!(key %in% names(inputs))) {
            inputs[[key]] <- character(0)
          }
          inputs[[key]] <- c(inputs[[key]], new_level)
        }
      }
      if (new_level == "high") {
        high_pulses <- high_pulses + length(nbrs)
      } else {
        low_pulses <- low_pulses + length(nbrs)
      }
    }
  }
  return(list(
    low_pulses = low_pulses,
    high_pulses = high_pulses,
    flip_flop_states = flip_flop_states,
    conjunction_states = conjunction_states,
    inputs = inputs
  ))
}

solve1 <- function(data) {
  graph_data <- build_graph(data)
  graph <- graph_data$graph
  flip_flop_states <- graph_data$flip_flop_states
  conjunction_states <- graph_data$conjunction_states
  export_graph(graph, file_name = "src/day20.png")

  high_pulses <- rep(as.bigz(0), 1000)
  low_pulses <- rep(as.bigz(0), 1000)
  for (i in 1:1000) {
    result <- push_btn(graph, flip_flop_states, conjunction_states)
    flip_flop_states <- result$flip_flop_states
    conjunction_states <- result$conjunction_states
    high_pulses[i] <- as.bigz(result$high_pulses)
    low_pulses[i] <- as.bigz(result$low_pulses)
  }
  cat(as.character(sum(high_pulses) * sum(low_pulses)), "\n")
}

# Key observation from day20.png: rx comes a single conjunction of four other
# conjunctions. We need each conjunction to send a single high pulse
solve2 <- function(data) {
  graph_data <- build_graph(data)
  graph <- graph_data$graph
  flip_flop_states <- graph_data$flip_flop_states
  conjunction_states <- graph_data$conjunction_states
  nodes <- get_node_df(graph)
  rx_parents <- get_predecessors(graph, which(nodes$label == "rx"))
  rx_grandparents <- sapply(rx_parents, function(x) {
    get_predecessors(graph, x)
  })
  first_time_single_high <- list()
  i <- 1
  repeat {
    result <- push_btn(graph, flip_flop_states, conjunction_states, rx_parents)
    flip_flop_states <- result$flip_flop_states
    conjunction_states <- result$conjunction_states
    inputs <- result$inputs
    for (p in rx_grandparents) {
      key <- as.character(p)
      if (sum(inputs[[key]] == "high") == 1) {
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
  cat(as.character(reduce(first_time_single_high, lcm.bigz)), "\n")
}
