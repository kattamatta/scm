
file <- "~/Downloads/SCM/tmp_outputfull.txt"

#' @export

read.scm4 <- function(file, ...) {
  
  
  input_lines <- readLines(con = file)
  row_numbers <- grep(input_lines, pattern = "^GROUP|^Number of isolates")
  
  skip <- row_numbers[-length(row_numbers)]
  n    <- diff(row_numbers) - 3L
  group_list <- Map(
    f = read.table
    , skip = skip
    , nrows = n
    , file = file
    # , widths = c(10, 20, 30, 20)
    , sep = "\t"
    , header = FALSE
  )
  
  y <- Map(
    x = group_list
    , i = seq_along(group_list)
    , f = function(x, i) {
      data.frame(
        file = basename(file)
        , group = i
        , name = trimws(gsub(x$V4, pattern = "name:", replacement = ""))
        , nominations = as.integer(trimws(gsub(x$V6, pattern = "nominations:", replacement = "")))
        , V8 = trimws(x$V8)
        , members = nrow(x)
      )
    }
  ) |>
    do.call(what = "rbind")
}
