library("gmp", include.only = "as.bigz")

parse_rule <- function(rule) {
  parts <- strsplit(rule, "{", fixed = TRUE)[[1]]
  name <- parts[1]
  dests <- strsplit(substr(parts[2], 1, nchar(parts[2]) - 1), ",")[[1]]
  dests <- sapply(dests, function(d) {
    if (!grepl(":", d)) {
      return(list(cond = "TRUE", target = d))
    }
    d <- strsplit(d, ":")[[1]]
    list(cond = d[1], target = d[2])
  })
  return(list(name = name, dests = t(as.data.frame(dests))))
}

parse_rating <- function(rating) {
  rating <- strsplit(substr(rating, 2, nchar(rating) - 1), ",")[[1]]
  res <- list()
  for (part in strsplit(rating, "=")) {
    res[[part[1]]] <- as.numeric(part[2])
  }
  return(res)
}

apply_rule <- function(rule, rating) {
  for (i in seq_len(nrow(rule))) {
    dest <- rule[i, ]
    if (eval(parse(text = dest$cond), envir = rating)) {
      return(dest$target)
    }
  }
  # Unreachable
  return(NULL)
}

solve1 <- function(data) {
  blocks <- strsplit(paste0(data, collapse = "\n"), "\n\n")[[1]]
  rules_list <- lapply(strsplit(blocks[1], "\n")[[1]], parse_rule)
  rules <- list()
  for (rule in rules_list) {
    rules[[rule$name]] <- rule$dests
  }
  ratings <- lapply(strsplit(blocks[2], "\n")[[1]], parse_rating)
  res <- sapply(ratings, function(r) {
    current <- "in"
    while (current != "A" && current != "R") {
      rule <- rules[[current]]
      current <- apply_rule(rule, r)
    }
    return(current == "A")
  })
  accepted <- ratings[res]
  cat(sum(unlist(accepted)), "\n")
}

negate_cond <- function(cond) {
  var <- substr(cond, 1, 1)
  op <- substr(cond, 2, 2)
  k <- as.integer(substr(cond, 3, nchar(cond)))
  if (op == "<") {
    return(paste0(var, ">", k - 1))
  } else if (op == ">") {
    return(paste0(var, "<", k + 1))
  }
}

get_paths <- function(rules) {
  paths <- list()
  out_i <- 0
  stack <- list(list(node = "A", path = NULL, visited = "A"))

  while (length(stack) > 0) {
    st <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    node <- st$node
    if (st$node == "in") {
      out_i <- out_i + 1
      paths[[out_i]] <- st$path[st$path != "TRUE"]
      next
    }
    node_rules <- rules[[node]]
    if (is.null(node_rules) || length(node_rules) == 0) {
      next
    }
    sources <- names(node_rules)
    for (s in sources) {
      if (s %in% st$visited) {
        next
      }
      edge_list <- node_rules[[s]]
      for (conds in edge_list) {
        stack[[length(stack) + 1L]] <- list(
          node = s,
          path = c(st$path, conds),
          visited = c(st$visited, s)
        )
      }
    }
  }

  return(paths)
}

get_cube <- function(inequalities) {
  ranges <- list(
    "x" = c(1, 4000),
    "m" = c(1, 4000),
    "a" = c(1, 4000),
    "s" = c(1, 4000),
    sign = 1
  )
  for (ineq in inequalities) {
    var <- substr(ineq, 1, 1)
    op <- substr(ineq, 2, 2)
    value <- as.numeric(substr(ineq, 3, nchar(ineq)))
    if (op == "<") {
      ranges[[var]][2] <- min(ranges[[var]][2], value - 1)
    } else if (op == ">") {
      ranges[[var]][1] <- max(ranges[[var]][1], value + 1)
    }
    if (ranges[[var]][1] > ranges[[var]][2]) {
      print(ranges[[var]])
    }
  }
  return(ranges)
}

intersect_cubes <- function(cube1, cube2, sign) {
  x1 <- max(cube1$x[1], cube2$x[1])
  x2 <- min(cube1$x[2], cube2$x[2])
  m1 <- max(cube1$m[1], cube2$m[1])
  m2 <- min(cube1$m[2], cube2$m[2])
  a1 <- max(cube1$a[1], cube2$a[1])
  a2 <- min(cube1$a[2], cube2$a[2])
  s1 <- max(cube1$s[1], cube2$s[1])
  s2 <- min(cube1$s[2], cube2$s[2])
  if (x1 > x2 || m1 > m2 || a1 > a2 || s1 > s2) {
    return(NULL)
  }
  return(list(
    x = c(x1, x2),
    m = c(m1, m2),
    a = c(a1, a2),
    s = c(s1, s2),
    sign = sign
  ))
}

add_cube <- function(cube, cubes) {
  new_cubes <- cubes
  new_cubes[[length(new_cubes) + 1]] <- cube
  for (c in cubes) {
    intersection <- intersect_cubes(cube, c, -c$sign)
    if (is.null(intersection)) {
      next
    }
    new_cubes[[length(new_cubes) + 1]] <- intersection
  }
  return(new_cubes)
}

solve2 <- function(data) {
  blocks <- strsplit(paste0(data, collapse = "\n"), "\n\n")[[1]]
  rules_list <- lapply(strsplit(blocks[1], "\n")[[1]], parse_rule)
  rules <- list()
  for (rule in rules_list) {
    rules[[rule$name]] <- rule$dests
  }
  reverse_rules <- list()
  for (rule in rules_list) {
    # If we took a branch, it means all earlier conditions must have failed.
    # So each edge doesn't just contain its own condition, but also the negation
    # of all previous conditions.
    prev_conds <- character(0)

    for (i in seq_len(nrow(rule$dests))) {
      target <- rule$dests[[i, "target"]]
      cond <- rule$dests[[i, "cond"]]
      conds <- prev_conds
      if (cond != "TRUE") {
        conds <- c(conds, cond)
      }
      if (is.null(reverse_rules[[target]])) {
        reverse_rules[[target]] <- list()
      }
      if (is.null(reverse_rules[[target]][[rule$name]])) {
        reverse_rules[[target]][[rule$name]] <- list()
      }
      all_paths <- reverse_rules[[target]][[rule$name]]
      reverse_rules[[target]][[rule$name]][[length(all_paths) + 1]] <- conds
      if (cond != "TRUE") prev_conds <- c(prev_conds, negate_cond(cond))
    }
  }
  paths <- get_paths(reverse_rules)
  cubes <- lapply(paths, get_cube)
  cubes_union <- list()
  for (cube in cubes) {
    cubes_union <- add_cube(cube, cubes_union)
  }
  volume <- as.bigz(0)
  for (cube in cubes_union) {
    volume <- volume +
      cube$sign *
        as.bigz(diff(cube$x) + 1) *
        as.bigz(diff(cube$m) + 1) *
        as.bigz(diff(cube$a) + 1) *
        as.bigz(diff(cube$s) + 1)
  }
  cat(as.character(volume), "\n")
}
