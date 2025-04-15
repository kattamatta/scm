#' calculate amount of social preferences  
#'
#' @description this function calculates how often a person (identifier) is listed as liked most and like least within school/stream
#'
#' @param data data frame containing social preference (aka identifier) data only, within preference identifiers are separated by whitespace. Variable names are "sch", "scm_class_nr", "scm_likemst", & ""scm_likelst" (NOTE: variable names without prefix "BA_", "FU1_", or "FU2_"; can be removed by `names(df) <- gsub("BA_", "", names(df))`)!
#' @param save logical. If `TRUE`, returned data frame will be saved as .txt with ; separation
#' @param file required if `save == TRUE`. `file` has to be absolute path in "" where returned data set should be stored.
#' @return extended input data frame containing variable `n_likemst` (amount how often identifier listed as most liked) and `n_likelst` (amount how often identifier listed as least liked)
#' @export


sps.calc <- function(data, save, file){
  sps <- cbind(data, n_likemst = NA_integer_, n_likelst = NA_integer_)
  for(i in unique(sps$sch)){
    tmp <- sps[sps$sch == i, ]
      for(j in tmp$scm_class_nr){
      tmp[tmp$scm_class_nr == j, "n_likemst"] <- sum(grepl(paste0(j), tmp$scm_likemst)== TRUE)
      tmp[tmp$scm_class_nr == j, "n_likelst"] <- sum(grepl(paste0(j), tmp$scm_likelst)== TRUE)
    }
    sps[sps$sch == i, ] <- tmp
  }
  assign("sps_calc", value = sps, envir = .GlobalEnv)
  if (save == "TRUE") {
    file <- paste(file)
    utils::write.csv2(sps, file = file, row.names = FALSE)
  } else {
  }
  print("successfully calculated sps amount")
}




         