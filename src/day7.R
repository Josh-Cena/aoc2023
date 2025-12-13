types <- list(
  "five of a kind" = 6,
  "four of a kind" = 5,
  "full house" = 4,
  "three of a kind" = 3,
  "two pair" = 2,
  "one pair" = 1,
  "high card" = 0
)

hand_type <- function(hand) {
  card_counts <- table(unlist(strsplit(hand, "")))
  max_count <- max(card_counts)
  if (max_count == 5) {
    return(types[["five of a kind"]])
  } else if (max_count == 4) {
    return(types[["four of a kind"]])
  } else if (max_count == 3 && length(card_counts) == 2) {
    return(types[["full house"]])
  } else if (max_count == 3) {
    return(types[["three of a kind"]])
  } else if (max_count == 2 && length(card_counts) == 3) {
    return(types[["two pair"]])
  } else if (max_count == 2) {
    return(types[["one pair"]])
  } else {
    return(types[["high card"]])
  }
}

hand_type_joker <- function(hand) {
  card_counts <- table(unlist(strsplit(hand, "")))
  j_count <- if ("J" %in% names(card_counts)) card_counts["J"] else 0
  card_counts <- card_counts[names(card_counts) != "J"]
  max_count <- (if (length(card_counts) > 0) max(card_counts) else 0) + j_count
  if (max_count == 5) {
    return(types[["five of a kind"]])
  } else if (max_count == 4) {
    return(types[["four of a kind"]])
  } else if (max_count == 3 && length(card_counts) == 2) {
    return(types[["full house"]])
  } else if (max_count == 3) {
    return(types[["three of a kind"]])
  } else if (max_count == 2 && length(card_counts) == 3) {
    return(types[["two pair"]])
  } else if (max_count == 2) {
    return(types[["one pair"]])
  } else {
    return(types[["high card"]])
  }
}

compare_rows <- function(strengths) {
  function(row1, row2) {
    if (row1$type > row2$type) {
      return(1)
    } else if (row1$type < row2$type) {
      return(-1)
    }
    cards1 <- unlist(strsplit(row1$hand, ""))
    cards2 <- unlist(strsplit(row2$hand, ""))
    for (i in seq_along(cards1)) {
      if (match(cards1[i], strengths) > match(cards2[i], strengths)) {
        return(1)
      } else if (match(cards1[i], strengths) < match(cards2[i], strengths)) {
        return(-1)
      }
    }
    return(0)
  }
}

solve1 <- function(data) {
  data <- data.frame(matrix(unlist(strsplit(data, " ")),
    ncol = 2, byrow = TRUE))
  data <- setNames(data, c("hand", "bid"))
  data$bid <- as.numeric(data$bid)
  data$type <- sapply(data$hand, hand_type)
  data_sorted <- quicksort(
    split(data, seq_len(nrow(data))),
    compare_rows(c("2", "3", "4", "5", "6", "7", "8",
                  "9", "T", "J", "Q", "K", "A"))
  )
  data <- do.call(rbind, data_sorted)
  total_score <- sum(seq_len(nrow(data)) * data$bid)
  cat(total_score, "\n")
}

solve2 <- function(data) {
  data <- data.frame(matrix(unlist(strsplit(data, " ")),
    ncol = 2, byrow = TRUE))
  data <- setNames(data, c("hand", "bid"))
  data$bid <- as.numeric(data$bid)
  data$type <- sapply(data$hand, hand_type_joker)
  print(data)
  data_sorted <- quicksort(
    split(data, seq_len(nrow(data))),
    compare_rows(c("J", "2", "3", "4", "5", "6", "7",
                   "8", "9", "T", "Q", "K", "A"))
  )
  data <- do.call(rbind, data_sorted)
  total_score <- sum(seq_len(nrow(data)) * data$bid)
  cat(total_score, "\n")
}
