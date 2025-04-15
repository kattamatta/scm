#' format peer nomination data for external software  
#'
#' @description this function prepares peer nomination data for the format required for external software data analysis
#'
#' @param data data frame containing peer nomination (aka identifier) data only, within peer group identifiers are separated by whitespace. Variable names are "sch", "scm_class_nr", "scm_owngrp", "scm_othgrp1", "scm_othgrp2", & "scm_othgrp3" (NOTE: variable names without prefix "BA_", "FU1_", or "FU2_"; can be removed by `names(df) <- gsub("BA_", "", names(df))`)!
#' @param save logical. If `TRUE`, returned data frame will be saved as .txt with ; separation
#' @param file required if `save == TRUE`. `file` has to be absolute path in "" where returned data set should be stored.
#' @return data frames of each school and stream separately
#' @export
#' @importFrom tidyr pivot_longer


scm.prep <- function(data, save, file){
  for(i in unique(data$sch)) {
    tmp <- subset(data, sch == i, select = c("scm_class_nr", "scm_owngrp", "scm_othgrp1", "scm_othgrp2", "scm_othgrp3"))
    if (sum((tmp$scm_class_nr > 1999)==TRUE)>=1){
      tmpstr1 <- subset(tmp, scm_class_nr < 2000)
      tmpstr1_long <- tidyr::pivot_longer(tmpstr1,
                                        cols = -c(scm_class_nr),
                                        values_to = "grpid",
                                        names_to = "grp")
      tmpstr1_long$grpid <- gsub(",|, | , |\n| |  | $", " ", tmpstr1_long$grpid)
      tmpstr1_long <- tmpstr1_long[!(tmpstr1_long$grp == "scm_owngrp" & !grepl("\\s", tmpstr1_long$grpid)), ]
      tmpstr1_long$grpid <- ifelse(tmpstr1_long$grp == "scm_owngrp", paste(tmpstr1_long$grpid, tmpstr1_long$scm_class_nr), tmpstr1_long$grpid)
      tmpstr1_long$grpid <- gsub("^ ", "", tmpstr1_long$grpid)
      tmpstr1_long <- tmpstr1_long[tmpstr1_long$grpid != "",]
      tmpstr1_long <- tmpstr1_long[tmpstr1_long$scm_class_nr != tmpstr1_long$grpid, ]
      tmpstr1_long$full <- paste(tmpstr1_long$scm_class_nr, tmpstr1_long$grpid)
      tmpstr1_long$full <- paste0("#", tmpstr1_long$full)
      tmpstr1_long <- subset(tmpstr1_long, select = "full")
      tmpstr1_long <- tmpstr1_long[!(tmpstr1_long$full == "#NA NA"), ]
      tmpstr1_long$full <- gsub(tmpstr1_long$full, pattern = "  ", replace = " ")
      if(nrow(tmpstr1_long)>0) {
        assign(paste0('scm_str1_sch', i), value = tmpstr1_long, envir = .GlobalEnv)
        if (save == "TRUE") {
          file <- paste(file)
          filenn <- paste0(paste0(file, paste0('scm_str1_sch', i)), ".txt")
          utils::write.table(assign(paste0('scm_str1_sch', i), value = tmpstr1_long, envir = .GlobalEnv), file = filenn, row.names = FALSE, col.names = FALSE, quote = FALSE)
        } else {
        }
      }
      else{}
      tmpstr2 <- subset(tmp, scm_class_nr >= 2000 & scm_class_nr < 3000)
      tmpstr2_long <- tidyr::pivot_longer(tmpstr2,
                                         cols = -c(scm_class_nr),
                                         values_to = "grpid",
                                         names_to = "grp")
      tmpstr2_long$grpid <- gsub(",|, | , |\n| |  | $", " ", tmpstr2_long$grpid)
      tmpstr2_long <- tmpstr2_long[!(tmpstr2_long$grp == "scm_owngrp" & !grepl("\\s", tmpstr2_long$grpid)), ]
      tmpstr2_long$grpid <- ifelse(tmpstr2_long$grp == "scm_owngrp", paste(tmpstr2_long$grpid, tmpstr2_long$scm_class_nr), tmpstr2_long$grpid)
      tmpstr2_long$grpid <- gsub("^ ", "", tmpstr2_long$grpid)
      tmpstr2_long <- tmpstr2_long[tmpstr2_long$grpid != "",]
      tmpstr2_long <- tmpstr2_long[tmpstr2_long$scm_class_nr != tmpstr2_long$grpid, ]
      tmpstr2_long$full <- paste(tmpstr2_long$scm_class_nr, tmpstr2_long$grpid)
      tmpstr2_long$full <- paste0("#", tmpstr2_long$full)
      tmpstr2_long <- subset(tmpstr2_long, select = "full")
      tmpstr2_long <- tmpstr2_long[!(tmpstr2_long$full == "#NA NA"), ]
      tmpstr2_long$full <- gsub(tmpstr2_long$full, pattern = "  ", replace = " ")
      if(nrow(tmpstr2_long)>0) {
        assign(paste0('scm_str2_sch', i), value = tmpstr2_long, envir = .GlobalEnv)
        if (save == "TRUE") {
          file <- paste(file)
          filennn <- paste0(paste0(file, paste0('scm_str2_sch', i)), ".txt")
          utils::write.table(assign(paste0('scm_str2_sch', i), value = tmpstr2_long, envir = .GlobalEnv), file = filennn, row.names = FALSE, col.names = FALSE, quote = FALSE)
        } else {
        }
      }
      else{}
      if (sum((tmp$scm_class_nr > 2999)==TRUE)>=1){
      tmpstr3 <- subset(tmp, scm_class_nr >= 3000  & scm_class_nr < 4000)
      tmpstr3_long <- tidyr::pivot_longer(tmpstr3,
                                         cols = -c(scm_class_nr),
                                         values_to = "grpid",
                                         names_to = "grp")
      tmpstr3_long$grpid <- gsub(",|, | , |\n| |  | $", " ", tmpstr3_long$grpid)
      tmpstr3_long <- tmpstr3_long[!(tmpstr3_long$grp == "scm_owngrp" & !grepl("\\s", tmpstr3_long$grpid)), ]
      tmpstr3_long$grpid <- ifelse(tmpstr3_long$grp == "scm_owngrp", paste(tmpstr3_long$grpid, tmpstr3_long$scm_class_nr), tmpstr3_long$grpid)
      tmpstr3_long$grpid <- gsub("^ ", "", tmpstr3_long$grpid)
      tmpstr3_long <- tmpstr3_long[tmpstr3_long$grpid != "",]
      tmpstr3_long <- tmpstr3_long[tmpstr3_long$scm_class_nr != tmpstr3_long$grpid, ]
      tmpstr3_long$full <- paste(tmpstr3_long$scm_class_nr, tmpstr3_long$grpid)
      tmpstr3_long$full <- paste0("#", tmpstr3_long$full)
      tmpstr3_long <- subset(tmpstr3_long, select = "full")
      tmpstr3_long <- tmpstr3_long[!(tmpstr3_long$full == "#NA NA"), ]
      tmpstr3_long$full <- gsub(tmpstr3_long$full, pattern = "  ", replace = " ")
      if(nrow(tmpstr3_long)>0) {
        assign(paste0('scm_str3_sch', i), value = tmpstr3_long, envir = .GlobalEnv)
        if (save == "TRUE") {
          file <- paste(file)
          filennn <- paste0(paste0(file, paste0('scm_str3_sch', i)), ".txt")
          utils::write.table(assign(paste0('scm_str3_sch', i), value = tmpstr3_long, envir = .GlobalEnv), file = filennn, row.names = FALSE, col.names = FALSE, quote = FALSE)
        } else {
        }
      }
      else{}
      }
    else{}
    if (sum((tmp$scm_class_nr > 3999)==TRUE)>=1){
    tmpstr4 <- subset(tmp, scm_class_nr >= 4000 & scm_class_nr < 5000)
    tmpstr4_long <- tidyr::pivot_longer(tmpstr4,
                                        cols = -c(scm_class_nr),
                                        values_to = "grpid",
                                        names_to = "grp")
    tmpstr4_long$grpid <- gsub(",|, | , |\n| |  | $", " ", tmpstr4_long$grpid)
    tmpstr4_long <- tmpstr4_long[!(tmpstr4_long$grp == "scm_owngrp" & !grepl("\\s", tmpstr4_long$grpid)), ]
    tmpstr4_long$grpid <- ifelse(tmpstr4_long$grp == "scm_owngrp", paste(tmpstr4_long$grpid, tmpstr4_long$scm_class_nr), tmpstr4_long$grpid)
    tmpstr4_long$grpid <- gsub("^ ", "", tmpstr4_long$grpid)
    tmpstr4_long <- tmpstr4_long[tmpstr4_long$grpid != "",]
    tmpstr4_long <- tmpstr4_long[tmpstr4_long$scm_class_nr != tmpstr4_long$grpid, ]
    tmpstr4_long$full <- paste(tmpstr4_long$scm_class_nr, tmpstr4_long$grpid)
    tmpstr4_long$full <- paste0("#", tmpstr4_long$full)
    tmpstr4_long <- subset(tmpstr4_long, select = "full")
    tmpstr4_long <- tmpstr4_long[!(tmpstr4_long$full == "#NA NA"), ]
    tmpstr4_long$full <- gsub(tmpstr4_long$full, pattern = "  ", replace = " ")
    if(nrow(tmpstr4_long)>0) {
      assign(paste0('scm_str4_sch', i), value = tmpstr4_long, envir = .GlobalEnv)
      if (save == "TRUE") {
        file <- paste(file)
        filennn <- paste0(paste0(file, paste0('scm_str4_sch', i)), ".txt")
        utils::write.table(assign(paste0('scm_str4_sch', i), value = tmpstr4_long, envir = .GlobalEnv), file = filennn, row.names = FALSE, col.names = FALSE, quote = FALSE)
      } else {
      }
    }
    }
    else{}
    if (sum((tmp$scm_class_nr > 4999)==TRUE)>=1){
    tmpstr5 <- subset(tmp, scm_class_nr >= 5000)
    tmpstr5_long <- tidyr::pivot_longer(tmpstr5,
                                         cols = -c(scm_class_nr),
                                         values_to = "grpid",
                                         names_to = "grp")
    tmpstr5_long$grpid <- gsub(",|, | , |\n| |  | $", " ", tmpstr5_long$grpid)
    tmpstr5_long <- tmpstr5_long[!(tmpstr5_long$grp == "scm_owngrp" & !grepl("\\s", tmpstr5_long$grpid)), ]
    tmpstr5_long$grpid <- ifelse(tmpstr5_long$grp == "scm_owngrp", paste(tmpstr5_long$grpid, tmpstr5_long$scm_class_nr), tmpstr5_long$grpid)
    tmpstr5_long$grpid <- gsub("^ ", "", tmpstr5_long$grpid)
    tmpstr5_long <- tmpstr5_long[tmpstr5_long$grpid != "",]
    tmpstr5_long <- tmpstr5_long[tmpstr5_long$scm_class_nr != tmpstr5_long$grpid, ]
    tmpstr5_long$full <- paste(tmpstr5_long$scm_class_nr, tmpstr5_long$grpid)
    tmpstr5_long$full <- paste0("#", tmpstr5_long$full)
    tmpstr5_long <- subset(tmpstr5_long, select = "full")
    tmpstr5_long <- tmpstr5_long[!(tmpstr5_long$full == "#NA NA"), ]
    tmpstr5_long$full <- gsub(tmpstr5_long$full, pattern = "  ", replace = " ")
    if(nrow(tmpstr5_long)>0) {
      assign(paste0('scm_str5_sch', i), value = tmpstr5_long, envir = .GlobalEnv)
      if (save == "TRUE") {
        file <- paste(file)
        filennn <- paste0(paste0(file, paste0('scm_str5_sch', i)), ".txt")
        utils::write.table(assign(paste0('scm_str5_sch', i), value = tmpstr5_long, envir = .GlobalEnv), file = filennn, row.names = FALSE, col.names = FALSE, quote = FALSE)
      } else {
      }
    }
    else{}
    }
    }
    else{
      tmpn_long <- tidyr::pivot_longer(tmp,
                                       cols = -c(scm_class_nr),
                                       values_to = "grpid",
                                       names_to = "grp")
      tmpn_long$grpid <- gsub(",|, | , |\n| |  | $", " ", tmpn_long$grpid)
      tmpn_long <- tmpn_long[!(tmpn_long$grp == "scm_owngrp" & !grepl("\\s", tmpn_long$grpid)), ]
      tmpn_long$grpid <- ifelse(tmpn_long$grp == "scm_owngrp", paste(tmpn_long$grpid, tmpn_long$scm_class_nr), tmpn_long$grpid)
      tmpn_long$grpid <- gsub("^ ", "", tmpn_long$grpid)
      tmpn_long <- tmpn_long[tmpn_long$grpid != "",]
      tmpn_long <- tmpn_long[tmpn_long$scm_class_nr != tmpn_long$grpid, ]
      tmpn_long$full <- paste(tmpn_long$scm_class_nr, tmpn_long$grpid)
      tmpn_long$full <- paste0("#", tmpn_long$full)
      tmpn_long <- subset(tmpn_long, select = "full")
      tmpn_long <- tmpn_long[!(tmpn_long$full == "#NA NA"), ]
      tmpn_long$full <- gsub(tmpn_long$full, pattern = "  ", replace = " ")
      assign(paste0('scm_sch', i), value = tmpn_long, envir = .GlobalEnv)
      if (save == "TRUE") {
        file <- paste(file)
        filen <- paste0(paste0(file, paste0('scm_sch', i)), ".txt")
        utils::write.table(assign(paste0('scm_sch', i), value = tmpn_long, envir = .GlobalEnv), file = filen, row.names = FALSE, col.names = FALSE, quote = FALSE)
      } else {
      }
    }
  } 
  print("successfully created data frames")
}
