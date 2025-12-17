library("collections", include.only = "stack")

# directions: U, D, L, R
dirs <- list(
  U = c(-1,  0),
  D = c( 1,  0),
  L = c( 0, -1),
  R = c( 0,  1)
)

key <- function(r, c) paste0(r, ",", c)
parse_key <- function(k) as.integer(strsplit(k, ",")[[1]])
opposite_slope <- function(d) switch(d, U = "v", D = "^", L = ">", R = "<")

export_graph <- function(graph, file_name) {
  # Unfortunately DiagrammeR doesn't work with graphs with vertex names,
  # so we have to convert it to a numeric graph.
  graph_for_dgr <- igraph::delete_vertex_attr(graph, "name")
  dgr <- DiagrammeR::from_igraph(graph_for_dgr)
  DiagrammeR::export_graph(dgr, file_name = file_name)
}

# A junction is a cell with wall neighbor number != 2,
# or a slope.
build_junction_graph <- function(grid, start, prob) {
  nr <- nrow(grid)
  nc <- ncol(grid)

  inside <- function(r, c) r >= 1 && r <= nr && c >= 1 && c <= nc

  out_neighbors <- function(k) {
    rc <- parse_key(k)
    r <- rc[1]
    c <- rc[2]
    ch <- grid[r, c]
    if (ch == "#") return(character(0))
    ds <- if (ch == "^") "U"
      else if (ch == "v") "D"
      else if (ch == "<") "L"
      else if (ch == ">") "R"
      else names(dirs)
    res <- character(0)
    for (d in ds) {
      dr <- dirs[[d]][1]
      dc <- dirs[[d]][2]
      r2 <- r + dr
      c2 <- c + dc
      if (inside(r2, c2) && grid[r2, c2] != "#") {
        res <- c(res, key(r2, c2))
      }
    }
    return(res)
  }

  is_junction <- function(r, c) {
    if (grid[r, c] == "#") return(FALSE)
    if (grid[r, c] %in% c("^", "v", "<", ">")) return(TRUE)
    wall_neighbors <- 0
    for (d in names(dirs)) {
      dr <- dirs[[d]][1]
      dc <- dirs[[d]][2]
      r2 <- r + dr
      c2 <- c + dc
      if (!inside(r2, c2) || grid[r2, c2] == "#")
        wall_neighbors <- wall_neighbors + 1
    }
    return(wall_neighbors != 2)
  }

  junctions <- character(0)
  for (r in 1:nr) {
    for (c in 1:nc) {
      if (is_junction(r, c)) junctions <- c(junctions, key(r, c))
    }
  }

  edges <- character(0)
  edges_len <- integer(0)

  for (u in junctions) {
    for (v0 in out_neighbors(u)) {
      prev <- u
      cur <- v0
      dist <- 1

      repeat {
        # Reached another junction
        if (cur %in% junctions) {
          edges <- c(edges, u, cur)
          edges_len <- c(edges_len, dist)
          break
        }
        nexts <- out_neighbors(cur)
        nexts <- nexts[nexts != prev]
        # Otherwise it should always be a junction
        stopifnot(length(nexts) == 1)
        prev <- cur
        cur <- nexts[1]
        dist <- dist + 1
      }
    }
  }

  graph <- igraph::make_empty_graph(n = length(junctions), directed = TRUE)
  graph <- igraph::set_vertex_attrs(graph, name = junctions, label = junctions)
  graph <- igraph::add_edges(graph, edges, length = edges_len, label = edges_len)

  # Prune the graph by removing single-direction nodes and nodes that can be
  # entered from 2 directions but only left from 1 (it's still single-direction
  # since you have to use one neighbor as the exit)
  for (key in junctions) {
    out_edges <- igraph::incident(graph, key, mode = "out")
    in_edges <- igraph::incident(graph, key, mode = "in")
    successors <- igraph::ends(graph, out_edges)[, 2]
    predecessors <- igraph::ends(graph, in_edges)[, 1]
    if (length(successors) == 1
        && length(predecessors) == 2
        && successors %in% predecessors) {
      in_edges <- in_edges[predecessors != successors]
      predecessors <- predecessors[predecessors != successors]
    }
    if (length(successors) == 1 && length(predecessors) == 1
        && successors != predecessors) {
      edge1_len <- igraph::E(graph)[in_edges[1]]$length
      edge2_len <- igraph::E(graph)[out_edges[1]]$length
      new_edge_len <- edge1_len + edge2_len
      graph <- igraph::add_edges(graph, c(predecessors, successors),
        length = new_edge_len, label = new_edge_len)
      graph <- igraph::delete_vertices(graph, key)
    }
  }
  return(graph)
}

longest_path_dag <- function(graph, start, end) {
  # It's a DAG with a chessboard pattern!
  export_graph(graph, "src/day23_1.png")
  topo <- igraph::topo_sort(graph, mode = "out")
  dist <- rep(-Inf, igraph::vcount(graph))
  start_id <- match(key(start[1], start[2]), igraph::V(graph)$name)
  end_id <- match(key(end[1], end[2]), igraph::V(graph)$name)
  dist[start_id] <- 0
  for (u in topo) {
    if (!is.finite(dist[u])) next
    out_edges <- igraph::incident(graph, igraph::V(graph)[u], mode = "out")
    if (length(out_edges) == 0) next
    targets <- igraph::ends(graph, out_edges, names = FALSE)[, 2]
    dist[targets] <-
      pmax(dist[targets], dist[u] + igraph::E(graph)[out_edges]$length)
  }

  dist[end_id]
}

longest_path <- function(graph, start, end) {
  graph <- igraph::as_undirected(graph, edge.attr.comb = "first")
  export_graph(graph, "src/day23_2.png")
  start_id <- match(key(start[1], start[2]), igraph::V(graph)$name)
  end_id <- match(key(end[1], end[2]), igraph::V(graph)$name)
  # Get adjacency list for DFS
  el <- igraph::as_edgelist(graph, names = FALSE)
  e_len <- igraph::edge_attr(graph, "length")
  n <- igraph::vcount(graph)
  m <- nrow(el)

  neighbors <- vector("list", n)
  edge_lens <- vector("list", n)
  for (i in seq_len(n)) {
    neighbors[[i]] <- integer(0)
    edge_lens[[i]] <- numeric(0)
  }

  for (k in seq_len(m)) {
    u <- el[k, 1]
    v <- el[k, 2]
    w <- e_len[k]
    neighbors[[u]] <- c(neighbors[[u]], v)
    edge_lens[[u]] <- c(edge_lens[[u]], w)
    neighbors[[v]] <- c(neighbors[[v]], u)
    edge_lens[[v]] <- c(edge_lens[[v]], w)
  }

  # Literally just DFS
  visited <- rep(FALSE, n)
  visited[start_id] <- TRUE
  st <- stack()
  st$push(list(vertex = start_id, neighbor = 1, len = 0))
  max_len <- 0
  while (st$size() > 0) {
    fr <- st$pop()
    u <- fr$vertex
    i <- fr$neighbor

    # Done with this vertex, backtrack
    if (i > length(neighbors[[u]])) {
      if (u != start_id) visited[u] <- FALSE
      next
    }

    st$push(list(vertex = u, neighbor = i + 1, len = fr$len))
    v <- neighbors[[u]][i]
    if (visited[v]) next

    new_len <- fr$len + edge_lens[[u]][i]
    if (v == end_id) {
      if (new_len > max_len) max_len <- new_len
    } else {
      visited[v] <- TRUE
      st$push(list(vertex = v, neighbor = 1, len = new_len))
    }
  }

  max_len
}

solve1 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  start <- c(1, 2)
  end <- c(nrow(mat), ncol(mat) - 1)
  graph <- build_junction_graph(mat, start = start, prob = "1")
  cat(longest_path_dag(graph, start = start, end = end), "\n")
}

solve2 <- function(data) {
  mat <- t(as.matrix(sapply(strsplit(data, ""), unlist)))
  mat[mat %in% c("^", "v", "<", ">")] <- "."
  start <- c(1, 2)
  end <- c(nrow(mat), ncol(mat) - 1)
  graph <- build_junction_graph(mat, start = start, prob = "2")
  cat(longest_path(graph, start = start, end = end), "\n")
}
