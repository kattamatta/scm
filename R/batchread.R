#' import and rowbinds multiple scm output files to one data frame with information on participant's & group's nomination, centrality, school/stream
#'
#' @description this function imports multiple scm output files, with information on participant's & group's nomination & centrality plus adds columns for school/stream information, then rowbinds data sets into one data frame
#'
#' @param path path to folder that holds all to be imported scm output data frames. Data frames are .txt and named in the general pattern "scm_str#_sch#.txt" or "scm_sch#.txt" , where # is to be replaced by number of specific school & stream. The single to be imported .txt files hold the results from the peer nomination analysis conducted in external sotware
#' @param save logical. If `TRUE`, returned data frame will be saved as .txt with ; separation
#' @param file required if `save == TRUE`. `file` has to be absolute path in "" where returned data set should be stored.
#' @return data frame of imported with information on participant's & group's nomination, centrality, school/stream
#' @export


batch.read <- function(path, save, file){
  filenames <- list.files(path = path, full.names = T)
tmp <- lapply(filenames, read.scm4) |>
  do.call(what = "rbind")

tmp$file <- gsub("scm_|\\.txt", "", tmp$file)

tmp2 <- strsplit(tmp$file, split = "_") |>
  lapply(function(x){
    if(length(x) != 1){
      gsub("str|sch", "", x)
    }else{
      c(gsub("sch", "", x), NA)
    }
  })

tmp$sch <- factor(vapply(tmp2, FUN = `[[`, FUN.VALUE = character(1), i = 1))
tmp$str <- factor(vapply(tmp2, FUN = `[[`, FUN.VALUE = character(1), i = 2))
tmp$file <- NULL
tmp$uuid <- NULL
assign("scm_merged_output", value = tmp, envir = .GlobalEnv)
if (save == "TRUE") {
  file <- paste(file)
  utils::write.csv2(sch_merged_output, file = file, row.names = FALSE)
} else {
}
print("successfully created data frame with merged scm results")

}
