#' import single scm output file with information on participant's & group's nomination, centrality plus participant being informant or non-informant
#'
#' @description this function imports a single scm output file, with information on participant's & group's nomination & centrality plus information whether participant is an informant or non-informant
#'
#' @param file path to folder that holds the single be imported scm output data frames. The data frame is a .txt and hold the results on participants only from the peer nomination analysis conducted in external software.
#' @param onlyinfo logical, `FALSE` (default) informants and non-informants will be included in the returned data frame, `TRUE` only informants will be included in the returned data frame
#' @return data frame with information on participant's & group's nomination, centrality, school/stream
#' \item{file}{name of input .txt-file}
#' \item{group}{ _GROUP_ or `isolates` in case of isolates}
#' \item{participant}{ _name_ }
#' \item{nominations}{participant's _nominations_ }
#' \item{centrality}{participant's _centrality_ }
#' \item{group_nominations}{group's _NOMINATION_ }
#' \item{group_centrality}{group's _CENTRALITY_ }
#' \item{n_members}{group's _MEMBERS_ or _Number of isolates_ in case of isolates}
#' \item{informant}{logical, `TRUE` indicates that participant is an informant, `FALSE` incidates that participant is a non-informant}
#' @export

read.scm4 <- function(file, onlyinfo = FALSE, ...) {
  
  # Input validation ----
  if(!length(onlyinfo) == 1L | anyNA(onlyinfo)) stop("Argument 'onlyinfo' must be TRUE or FALSE.")
  onlyinfo <- isTRUE(onlyinfo)
  
  
  input_lines <- readLines(con = file)
  group_rows <- grep(input_lines, pattern = "^GROUP")
  isolates_row <- grep(input_lines, pattern = "^Number of isolates")
  informant_rows <- grep(input_lines, pattern = "Informant:")
  
  
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
  
  group_info <- strsplit(input_lines[group_rows], split = "MEMBERS|NOMINATIONS:|CENTRALITY:") |>
    lapply(trimws) |>
    lapply(function(x){
      list(
        nominations = as.integer(x[[3L]])
        , centrality = tolower(x[[4L]])
        , isolates = FALSE
      )
    })
  
  informant_info <- strsplit(input_lines[informant_rows], split = "Informant:") |>
    lapply(trimws) |>
    lapply(function(x){
        informant = as.integer(x[[2L]])
    })|>
    unlist() |>
    unique()
  
  n_isolates <- as.integer(gsub(input_lines[isolates_row], pattern = ".* = ", replacement = ""))
  
  if(n_isolates > 0) {
    isolates_data <- read.table(file = file, skip = isolates_row, nrows = n_isolates, sep = "\t", header = FALSE) |>
      within({
        V8 <- NA
      })
    
    group_list <- c(group_list, list(isolates_data))
    group_info <- c(group_info, list(list(nominations = NA, centrality = NA, isolates = TRUE)))
  }
  
  y <- Map(
    x = group_list
    , i = seq_along(group_list)
    , group_info = group_info
    # , isolates = isolates
    , f = function(
      x
      , i
      , group_info
      # , isolates
    ) {
      data.frame(
        file = basename(file)
        , group = if(group_info$isolates) "isolates" else as.character(i)
        , participant = trimws(gsub(x$V4, pattern = "name:", replacement = ""))
        , nominations = as.integer(trimws(gsub(x$V6, pattern = "nominations:", replacement = "")))
        , centrality = tolower(trimws(x$V8))
        , group_nominations = group_info$nominations
        , group_centrality = group_info$centrality
        , n_members = nrow(x)
        # , n_isolates = isolates
        # , uuid = uuid::UUIDgenerate(n = 1L)
      )
    }
  ) |>
    do.call(what = "rbind") |>
    within({
      #file_uuid <- uuid::UUIDgenerate(n = 1L)
    })
  
  y$informant <- y$participant %in% informant_info
  
  if(onlyinfo){
    y <- subset(y, informant)
  }
  
  # return
  y
}
