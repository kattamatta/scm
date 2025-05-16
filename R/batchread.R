#' import and rowbinds multiple scm output files to one data frame with information on informant's & group's nomination, centrality, school/stream plus participant being informant or non-informant
#'
#' @description this function imports multiple scm output files, with information on informant's & group's nomination & centrality plus information whether participant is an informant or non-informant plus adds columns for school/stream information, then rowbinds data sets into one data frame
#'
#' @param path path to folder that holds all to be imported scm output data frames. Data frames are .txt and named in the general pattern "scm_str#_sch#.txt" or "scm_sch#.txt" , where # is to be replaced by number of specific school & stream. The single to be imported .txt files hold the results from the peer nomination analysis conducted in external software. In case of sch#.txt (without stream), NA will be added to `str`.
#' @param save logical. If `TRUE`, returned data frame will be saved as .txt with ; separation
#' @param file required if `save == TRUE`. `file` has to be absolute path in "" where returned data set should be stored.
#' @param onlyinfo logical, `FALSE` (default) informants and non-informants will be included in the returned data frame, `TRUE` only informants will be included in the returned data frame
#' @return data frame of all imported files containing variables:
#' \item{file}{name of input .txt-file}
#' \item{group}{ _GROUP_ or `isolates` in case of isolates}
#' \item{informant}{ _name_ }
#' \item{nominations}{informant's _nominations_ }
#' \item{centrality}{informant's _centrality_ }
#' \item{group_nominations}{group's _NOMINATION_ }
#' \item{group_centrality}{group's _CENTRALITY_ }
#' \item{n_memebers}{group's _MEMBERS_ or _Number of isolates_ in case of isolates}
#' \item{informant}{logical, `TRUE` indicates that participant is an informant, `FALSE` incidates that participant is a non-informant}
#' \item{str}{stream from .txt-file name or `NA` in case of non stream}
#' \item{sch}{school from .txt-file name}
#' 
#' @export

batch.read <- function(path, save, file, onlyinfo = FALSE, ...){
  
  filenames <- list.files(path = path, full.names = T)
  
  tmp <- lapply(filenames, read.scm4, onlyinfo = onlyinfo) |>
    do.call(what = "rbind")

tmp$file <- gsub("scm_|\\.txt", "", tmp$file)

tmp2 <- strsplit(tmp$file, split = "_") |>
  lapply(function(x){
    if(length(x) != 1){
      gsub("str|sch", "", x)
    }else{
      c(NA, gsub("sch", "", x))
    }
  })

tmp$str <- as.character(factor(vapply(tmp2, FUN = `[[`, FUN.VALUE = character(1), i = 1)))
tmp$sch <- as.character(factor(vapply(tmp2, FUN = `[[`, FUN.VALUE = character(1), i = 2)))
#tmp$file <- NULL
tmp$file_uuid <- NULL
assign("scm_merged_output", value = tmp, envir = .GlobalEnv)
if (save == "TRUE") {
  file <- paste(file)
  utils::write.csv2(scm_merged_output, file = file, row.names = FALSE)
} else {
}
print("successfully created data frame with merged scm results")

}
