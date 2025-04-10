
batch.read <- function(path){
  filenames <- list.files(path = path, full.names = T)
tmp <- lapply(filenames, read.scm4) |>
  do.call(what = "rbind")

tmp$file <- gsub("scm_|\\.txt", "", tmp$file)

tmp2 <- strsplit(tmp$file, split = "_") |>
  lapply(function(x){
    if(length(x) == 1){
      c(gsub("sch", "", x), NA)
    }else{
      gsub("sch|str", "", x)
    }
  })

tmp$sch <- factor(vapply(tmp2, FUN = `[[`, FUN.VALUE = character(1), i = 1))
tmp$str <- factor(vapply(tmp2, FUN = `[[`, FUN.VALUE = character(1), i = 2))
tmp$file <- NULL
tmp$uuid <- NULL
tmp
}
