#' @param x A data frame of including columns id, date and prcp
#' @param thr A named vecter of threshold where the names are statioin ids that can be find in x$id
#' @param nbr A named list of neighbouring station ids where the names are statioin ids that can be find in x$id
find_extreme <- function(x, thr, nbr) {
  # library(tidyverse)
  # library(rlang)
  # library(targets)
  # library(Matrix)
  # x <- tar_read(df_prcp)
  # thr <- tar_read(vec_thr)
  # nbr <- tar_read(ls_nbr)

  # id <- "ASN00040452"
  # period <- c(ymd("1974-01-25"), ymd("1974-01-29"))
  # x <- filter(df_prcp, id %in% c(id, ls_nbr[[.env$id]]), between(date, period[[1]], period[[2]]))

  df_prcp <- x
  x <- df_prcp[!is.na(df_prcp$prcp), ]
  x <- filter(x, prcp > 0)

  all_date <- sort(unique(df_prcp$date))
  all_id <- sort(unique(df_prcp$id))
  # id %in% all_id
  mat <- make_sparse(x, date, id, prcp, row_value = all_date, col_value = all_id)
  # nbr_mat <- make_sparse(nbr, row_value = sort(unique(x$id)))
  nbr <- nbr[names(nbr) %in% all_id]
  # id %in% names(nbr)
  nbr_idx <- lapply(nbr, \(x) na.omit(match(x, all_id)))
  # id %in% names(nbr_idx)
  # nbr_idx[[id]]

  nbr_max <- lapply(nbr_idx, \(id) qlcMatrix::rowMax(mat[, id, drop = FALSE]))
  # any(nbr_max[[id]] > 0)
  out <- nbr_max %>%
    lapply(\(x) {
      idx_nonzero <- which(as(x, "lsparseVector"))
      data.frame(max_prcp = as.vector(x[idx_nonzero]), date = all_date[idx_nonzero])
    }) %>%
    bind_rows(.id = "id")
  out <- out %>%
    mutate(thr = thr[match(.data$id, names(thr))]) %>%
    mutate(extreme = max_prcp > thr) %>%
    # select(id, extreme, date) %>%
    # filter(extreme) %>%
    left_join(df_prcp, ., by = join_by(id, date)) %>%
    replace_na(list(extreme = FALSE))
  # filter(out, id == .env$id, between(date, period[[1]], period[[2]]))
  out
}
# filter(tar_read(df_prcp_ext), id == .env$id, between(date, period[[1]], period[[2]]))


# @param row name of the column that are taken to be the row index
# @param col name of the column that are taken to be the column index
# @param value name of the column that are taken to be the entries of the matrix
# @importFrom rlang expr as_names
make_sparse <- function(x, ...) {
  UseMethod("make_sparse")
}

# @param row_value values used for the row names
make_sparse.list <- function(x, row_value = sort(unique(unlist(x))), ...) {
  x_length <- vapply(x, length, FUN.VALUE = integer(1L))

  i <- match(unlist(x), row_value)
  j <- rep(seq_along(x), x_length)

  Matrix::sparseMatrix(
    i = i[!is.na(i)], j = j[!is.na(i)], x = 1L,
    dimnames = list(row_value, names(x))
  )
}

make_sparse.data.frame <- function(x, row, col, value, row_value = NULL, col_value = NULL) {
  row_name <- as_name(enexpr(row))
  col_name <- as_name(enexpr(col))
  value_name <- as_name(enexpr(value))

  # row_name <- "date"
  # col_name <- "id"
  # value_name <- "prcp"

  if (is.null(row_value)) {
    row_value <- sort(unique(x[[row_name]]))
  }
  if (is.null(col_value)) {
    col_value <- sort(unique(x[[col_name]]))
  }

  i <- match(x[[row_name]], row_value)
  j <- match(x[[col_name]], col_value)


  Matrix::sparseMatrix(
    i = i, j = j, x = x[[value_name]],
    dims = c(length(row_value), length(col_value)),
    dimnames = list(row_value, col_value)
  )
}

# count cumulative number of nonzero
# reset when there is zero
cumn_nonzero <- function(x) {
  r <- rle(x != 0)
  ls_r <- mapply(\(l, v) integer(l) + v, r$lengths, r$values,
    SIMPLIFY = FALSE
  )
  ls_r[r$values] <- lapply(ls_r[r$values], cumsum)
  unlist(ls_r)
}
