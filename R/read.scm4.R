
file <- "~/Downloads/SCM/tmp_outputfull.txt"

#' @export

read.scm4 <- function(file, ...) {
  
  
  input_lines <- readLines(con = file)
  group_rows <- grep(input_lines, pattern = "^GROUP")
  isolates_row <- grep(input_lines, pattern = "^Number of isolates")
  
  
  skip <- group_rows
  n    <- diff(c(group_rows, isolates_row)) - 3L
  group_list <- Map(
    f = read.table
    , skip = skip
    , nrows = n
    , file = file
    # , widths = c(10, 20, 30, 20)
    , sep = "\t"
    , header = FALSE
  )
  
  # todo: add group stats (from ^GROUP lines, nominations, )
  group_info <- strsplit(input_lines[group_rows], split = "MEMBERS|NOMINATIONS:|CENTRALITY:") |>
    lapply(trimws) |>
    lapply(function(x){
      list(
        nominations = as.integer(x[[3L]])
        , centrality = tolower(x[[4L]])
      )
    })
  
  isolates <- as.integer(gsub(input_lines[isolates_row], pattern = ".* = ", replacement = ""))
  
  y <- Map(
    x = group_list
    , i = seq_along(group_list)
    , group_info = group_info
    , isolates = isolates
    , f = function(x, i, group_info, isolates) {
      data.frame(
        file = basename(file)
        , group = i
        , participant = trimws(gsub(x$V4, pattern = "name:", replacement = ""))
        , nominations = as.integer(trimws(gsub(x$V6, pattern = "nominations:", replacement = "")))
        , centrality = tolower(trimws(x$V8))
        , group_nominations = group_info$nominations
        , group_centrality = group_info$centrality
        , n_members = nrow(x)
        , n_isolates = isolates
        , uuid = uuid::UUIDgenerate(n = 1L)
      )
    }
  ) |>
    do.call(what = "rbind")
  
  # return
  y
}
