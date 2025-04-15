#' extract rows of data frame where peer nomination identifier match required pattern of identifier
#'
#' @description this function filters for mismatching identifier pattern (4 digits) and extracts rows with least one pattern-mismatching identifier. Identifiers from same peer group will be listed separately column wise.
#'
#' @param data data frame containing peer nomination (aka identifier) data only, within peer group identifiers are separated by whitespace. Variable names are "sch", "scm_class_nr", "scm_owngrp", "scm_othgrp1", "scm_othgrp2", "scm_othgrp3", "scm_likemst", & ""scm_likelst" (NOTE: variable names without prefix "BA_", "FU1_", or "FU2_"; can be removed by `names(df) <- gsub("BA_", "", names(df))`)!
#' @param save logical. If `TRUE`, returned data frame will be saved as .csv with ; separation
#' @param file required if `save == TRUE`. `file` has to be absolute path including file name and appendix .csv in "" where returned data set should be stored.
#' @return data frames with not matching pattern
#' @export
#' @importFrom stringr str_count
#' @importFrom tidyr separate
#' @importFrom dplyr %>%


qcscm.filtering <- function(data, save, file){
  
  ## filter non alphanumeric characters
  dfc <- data[grep("[[:alpha:]]+", data[["scm_class_nr"]]), c(1:2)]
  dfc <- rbind(dfc, data[grep("[[:alpha:]]+", data[["scm_owngrp"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:punct:]]+", data[["scm_owngrp"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:alpha:]]+", data[["scm_othgrp1"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:punct:]]+", data[["scm_othgrp1"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:alpha:]]+", data[["scm_othgrp2"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:punct:]]+", data[["scm_othgrp2"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:alpha:]]+", data[["scm_othgrp3"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:punct:]]+", data[["scm_othgrp3"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:alpha:]]+", data[["scm_likemst"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:punct:]]+", data[["scm_likemst"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:alpha:]]+", data[["scm_likelst"]]), c(1:2)])
  dfc <- rbind(dfc, data[grep("[[:punct:]]+", data[["scm_likelst"]]), c(1:2)])
  dfc <- subset(dfc, !duplicated(dfc))
  dfc$pat <- rep("no numeric character(s)", nrow(dfc))
  names(dfc)[2] <- "scm_class_nr_1"
  
  ## duplicated sch*scm_owngrp
  dfdup <- data[duplicated(data[c(1:2)]), c(1:2)]
  dfdup$pat <- rep("duplicates sch+scm_owngrp", nrow(dfdup))
  names(dfdup)[2] <- "scm_class_nr_1"
  
  ## na coding
  dfna <- data[grep("777|7777|777777", data[["scm_class_nr"]]), c(1:2)]
  dfna <- rbind(dfna, data[grep("777|7777|777777", data[["scm_owngrp"]]), c(1:2)])
  dfna <- rbind(dfna, data[grep("777|7777|777777", data[["scm_othgrp1"]]), c(1:2)])
  dfna <- rbind(dfna, data[grep("777|7777|777777", data[["scm_othgrp2"]]), c(1:2)])
  dfna <- rbind(dfna, data[grep("777|7777|777777", data[["scm_othgrp3"]]), c(1:2)])
  dfna <- rbind(dfna, data[grep("777|7777|777777", data[["scm_likemst"]]), c(1:2)])
  dfna <- rbind(dfna, data[grep("777|7777|777777", data[["scm_likelst"]]), c(1:2)])
  dfna <- subset(dfna, !duplicated(dfna))
  dfna$pat <- rep("7777 coding for na", nrow(dfna))
  names(dfna)[2] <- "scm_class_nr_1"
  
  ## filter mismatiching 4 ditig pattern
  tmp <- data %>%
    mutate_at(vars(matches("^scm")), ~ sapply(., gsub, pattern = "o", replacement = "0", USE.NAMES = FALSE)) %>%
    mutate_at(vars(matches("^scm")), ~ sapply(., gsub, pattern = "\n", replacement = " ", USE.NAMES = FALSE)) %>%
    mutate_at(vars(matches("^scm")), ~ sapply(., gsub, pattern = "[[:alpha:]]+", replacement = "", USE.NAMES = FALSE)) %>%
    mutate_at(vars(matches("^scm_")), ~ sapply(., gsub, pattern = "[[:punct:]]", replacement = "", USE.NAMES = FALSE)) %>%
    mutate_at(vars(matches("^scm_")), ~ sapply(., gsub, pattern = "777|7777|777777|^0$", replacement = "", USE.NAMES = FALSE)) %>%
    mutate_at(vars(matches("^scm_")), ~ sapply(., gsub, pattern = "\\s\\s+", replacement = " ", USE.NAMES = FALSE)) %>%
    mutate_at(vars(matches("^scm_")), ~ replace(., is.na(.),""))
  
  tmp <- tmp %>%
    tidyr::separate(col = "scm_owngrp"
                    , into = paste0("scm_owngrp_", c(1:max(stringr::str_count(.[["scm_owngrp"]], " ") %>% 
                                                             replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right") %>%
    tidyr::separate(col = "scm_class_nr"
                    , into = paste0("scm_class_nr_", c(1:max(stringr::str_count(.[["scm_class_nr"]], " ") %>% 
                                                               replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right") %>%
    tidyr::separate(col = "scm_othgrp1"
                    , into = paste0("scm_othgrp1_", c(1:max(stringr::str_count(.[["scm_othgrp1"]], " ") %>% 
                                                              replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right") %>%
    tidyr::separate(col = "scm_othgrp2"
                    , into = paste0("scm_othgrp2_", c(1:max(stringr::str_count(.[["scm_othgrp2"]], " ") %>% 
                                                              replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right") %>%
    tidyr::separate(col = "scm_othgrp3"
                    , into = paste0("scm_othgrp3_", c(1:max(stringr::str_count(.[["scm_othgrp3"]], " ") %>% 
                                                              replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right") %>%
    tidyr::separate(col = "scm_likemst"
                    , into = paste0("scm_likemst_", c(1:max(stringr::str_count(.[["scm_likemst"]], " ") %>% 
                                                              replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right") %>%
    tidyr::separate(col = "scm_likelst"
                    , into = paste0("scm_likelst_", c(1:max(stringr::str_count(.[["scm_likelst"]], " ") %>% 
                                                              replace(is.na(.), 0)+1)))
                    , sep = " "
                    , fill = "right")
  
  tmp[tmp == 0] <- NA
  tmp[tmp == ""] <- NA

  tmp_red <- tmp %>% 
    rowwise %>%
    filter(if_any(c(2:length(.)), ~ nchar(., keepNA = T) != 4)[1]) %>%
    ungroup
  dfd <- subset(tmp_red, select = c("sch", "scm_class_nr_1"))
  dfd$pat <- rep("no 4-ditig pattern", nrow(dfd))
  
  ## duplicates within group
  dfdiown <- tmp[, grepl("sch|scm_class_nr_1|scm_owngrp_*", names(tmp))]
  dfdiown <- dfdiown[!duplicated(dfdiown[c(1:2)]),]
  dfdiown$scm_class_nr_1_rep <- dfdiown$scm_class_nr_1
  dfdiown <- dfdiown %>%
    gather(question, value, -c(sch, scm_class_nr_1)) %>% 
    group_by(sch, scm_class_nr_1) %>%
    count(value) %>% 
    ungroup() 
 dfdiown <- dfdiown[!is.na(dfdiown$value),]
 dfdiown <- dfdiown[dfdiown$n > 1,]
 dfdiown$pat <- paste0(dfdiown$n, sep = "x same identifier ", dfdiown$value, " in scm_owngrp")
 dfdiown <- dfdiown[, c(1,2,length(dfdiown))]

 dfdiothgrp1 <- tmp[, grepl("sch|scm_class_nr_1|scm_othgrp1_*", names(tmp))]
 dfdiothgrp1 <- dfdiothgrp1[!duplicated(dfdiothgrp1[c(1:2)]),]
 dfdiothgrp1 <- dfdiothgrp1 %>%
   gather(question, value, -c(sch, scm_class_nr_1)) %>% 
   group_by(sch, scm_class_nr_1) %>%
   count(value) %>% 
   ungroup() 
 dfdiothgrp1 <- dfdiothgrp1[!is.na(dfdiothgrp1$value),]
 dfdiothgrp1 <- dfdiothgrp1[dfdiothgrp1$n > 1,]
 dfdiothgrp1$pat <- paste0(dfdiothgrp1$n, sep = "x same identifier ", dfdiothgrp1$value, " in scm_othgrp1")
 dfdiothgrp1 <- dfdiothgrp1[, c(1,2,length(dfdiothgrp1))]

 dfdiothgrp2 <- tmp[, grepl("sch|scm_class_nr_1|scm_othgrp2_*", names(tmp))]
 dfdiothgrp2 <- dfdiothgrp2[!duplicated(dfdiothgrp2[c(1:2)]),]
 dfdiothgrp2 <- dfdiothgrp2 %>%
   gather(question, value, -c(sch, scm_class_nr_1)) %>% 
   group_by(sch, scm_class_nr_1) %>%
   count(value) %>% 
   ungroup() 
 dfdiothgrp2 <- dfdiothgrp2[!is.na(dfdiothgrp2$value),]
 dfdiothgrp2 <- dfdiothgrp2[dfdiothgrp2$n > 1,]
 dfdiothgrp2$pat <- paste0(dfdiothgrp2$n, sep = "x same identifier ", dfdiothgrp2$value, " in scm_othgrp2")
 dfdiothgrp2 <- dfdiothgrp2[, c(1,2,length(dfdiothgrp2))]

 dfdiothgrp3 <- tmp[, grepl("sch|scm_class_nr_1|scm_othgrp3_*", names(tmp))]
 dfdiothgrp3 <- dfdiothgrp3[!duplicated(dfdiothgrp3[c(1:2)]),]
 dfdiothgrp3 <- dfdiothgrp3 %>%
   gather(question, value, -c(sch, scm_class_nr_1)) %>% 
   group_by(sch, scm_class_nr_1) %>%
   count(value) %>% 
   ungroup() 
 dfdiothgrp3 <- dfdiothgrp3[!is.na(dfdiothgrp3$value),]
 dfdiothgrp3 <- dfdiothgrp3[dfdiothgrp3$n > 1,]
 dfdiothgrp3$pat <- paste0(dfdiothgrp3$n, sep = "x same identifier ", dfdiothgrp3$value, " in scm_othgrp3")
 dfdiothgrp3 <- dfdiothgrp3[, c(1,2,length(dfdiothgrp3))]

 dfdilikemst <- tmp[, grepl("sch|scm_class_nr_1|scm_likemst_*", names(tmp))]
 dfdilikemst <- dfdilikemst[!duplicated(dfdilikemst[c(1:2)]),]
 dfdilikemst <- dfdilikemst %>%
   gather(question, value, -c(sch, scm_class_nr_1)) %>% 
   group_by(sch, scm_class_nr_1) %>%
   count(value) %>% 
   ungroup() 
 dfdilikemst <- dfdilikemst[!is.na(dfdilikemst$value),]
 dfdilikemst <- dfdilikemst[dfdilikemst$n > 1,]
 dfdilikemst$pat <- paste0(dfdilikemst$n, sep = "x same identifier ", dfdilikemst$value, " in scm_likemst")
 dfdilikemst <- dfdilikemst[, c(1,2,length(dfdilikemst))]

 dfdilikelst <- tmp[, grepl("sch|scm_class_nr_1|scm_likelst_*", names(tmp))]
 dfdilikelst <- dfdilikelst[!duplicated(dfdilikelst[c(1:2)]),]
 dfdilikelst <- dfdilikelst %>%
   gather(question, value, -c(sch, scm_class_nr_1)) %>% 
   group_by(sch, scm_class_nr_1) %>%
   count(value) %>% 
   ungroup() 
 dfdilikelst <- dfdilikelst[!is.na(dfdilikelst$value),]
 dfdilikelst <- dfdilikelst[dfdilikelst$n > 1,]
 dfdilikelst$pat <- paste0(dfdilikelst$n, sep = "x same identifier ", dfdilikelst$value, " in scm_likelst")
 dfdilikelst <- dfdilikelst[, c(1,2,length(dfdilikelst))]
  
  ## > 20 identifiers within group
 df19own <- tmp[, grepl("sch|scm_class_nr_1|scm_owngrp_*", names(tmp))]
 df19own <- df19own[(rowSums(!is.na(df19own[c(3:length(df19own))])) > 19) == TRUE, c("sch", "scm_class_nr_1")]
 df19own$pat <- rep("> 19 identifiers in scm_owngrp", nrow(df19own))
 
 df20othgrp1 <- tmp[, grepl("sch|scm_class_nr_1|scm_othgrp1_*", names(tmp))]
 df20othgrp1 <- df20othgrp1[(rowSums(!is.na(df20othgrp1[c(3:length(df20othgrp1))])) > 20) == TRUE, c("sch", "scm_class_nr_1")]
 df20othgrp1$pat <- rep("> 20 identifiers in scm_othgrp1", nrow(df20othgrp1))
 
 df20othgrp2 <- tmp[, grepl("sch|scm_class_nr_1|scm_othgrp2_*", names(tmp))]
 df20othgrp2 <- df20othgrp2[(rowSums(!is.na(df20othgrp2[c(3:length(df20othgrp2))])) > 20) == TRUE, c("sch", "scm_class_nr_1")]
 df20othgrp2$pat <- rep("> 20 identifiers in scm_othgrp2", nrow(df20othgrp2))
 
 df20othgrp3 <- tmp[, grepl("sch|scm_class_nr_1|scm_othgrp3_*", names(tmp))]
 df20othgrp3 <- df20othgrp3[(rowSums(!is.na(df20othgrp3[c(3:length(df20othgrp3))])) > 20) == TRUE, c("sch", "scm_class_nr_1")]
 df20othgrp3$pat <- rep("> 20 identifiers in scm_othgrp3", nrow(df20othgrp3))
 
 df20likemst <- tmp[, grepl("sch|scm_class_nr_1|scm_likemst_*", names(tmp))]
 df20likemst <- df20likemst[(rowSums(!is.na(df20likemst[c(3:length(df20likemst))])) > 20) == TRUE, c("sch", "scm_class_nr_1")]
 df20likemst$pat <- rep("> 20 identifiers in scm_likemst", nrow(df20likemst))
 
 df20likelst <- tmp[, grepl("sch|scm_class_nr_1|scm_likelst_*", names(tmp))]
 df20likelst <- df20likelst[(rowSums(!is.na(df20likelst[c(3:length(df20likelst))])) > 20) == TRUE, c("sch", "scm_class_nr_1")]
 df20likelst$pat <- rep("> 20 identifiers in scm_likelst", nrow(df20likelst))
 
  ## outlier identifiers
  tmp[is.na(tmp)] <- 0
  tmp[tmp == ""] <- 0
  tmp <- as.data.frame(lapply(tmp, as.double, USE.NAMES = FALSE))
  tmp[is.na(tmp)] <- 0
  tmp[tmp == 0] <- NA
  
  IsOutlier <- function(x) {
    lowerq = quantile(x, na.rm = TRUE)[2]
    upperq = quantile(x, na.rm = TRUE)[4]
    iqr = upperq - lowerq 
    threshold_upper = (iqr * 1.5) + upperq
    threshold_lower = lowerq - (iqr * 1.5)
    x > threshold_upper | x <  threshold_lower 
  }
  
  tmp_red2 <- tmp[rowSums(sapply(tmp[, c(2:length(tmp))], IsOutlier, USE.NAMES = FALSE), na.rm = TRUE) > 0, ]
  dfo <- subset(tmp_red2, select = c("sch", "scm_class_nr_1"))
  dfo$pat <- rep("outlier identifier", nrow(dfo))
  
  ## combine data 
  df_all <- rbind(dfc, dfna, dfdup, dfd, dfo, dfdiown, dfdiothgrp1, dfdiothgrp2, dfdiothgrp3, dfdilikemst, dfdilikelst, df19own, df20othgrp1, df20othgrp2, df20othgrp3, df20likemst, df20likelst)
  df_all$schclass <- paste(df_all$sch, df_all$scm_class_nr_1)
  duplcode2_c <- subset(df_all$schclass, duplicated(df_all$schclass))
  if(length(duplcode2_c) != 0){
    dffilt2_c <- df_all[df_all$schclass %in% duplcode2_c, ]
    pattmp_c <- stats::as.formula(paste(quote(pat), quote(schclass), sep = " ~ "))
    listtmp_c <- list(x = pattmp_c, data = unique(dffilt2_c), FUN = paste, collapse = ", ")
    dffilt3_c <- do.call("aggregate", listtmp_c)
    dffilt2_c$pat2 <- dffilt3_c$pat[match(dffilt2_c$schclass, dffilt3_c$schclass)]
    df_all$pat2 <- dffilt2_c$pat2[match(df_all$schclass, dffilt2_c$schclass)]
    df_all$pat2 <- ifelse(is.na(df_all$pat2) == TRUE, df_all$pat, df_all$pat2)
    df_all$pat <- df_all$pat2
    df_all$pat2 <- NULL
    df_all <- df_all[!duplicated(df_all), ]
    df_all$schclass <- NULL
    names(df_all)[2] <- "scm_class_nr"
} else {
}
  
  assign("QC_scm", value = df_all, envir = .GlobalEnv)
  if(nrow(QC_scm) > 0){
    if (save == "TRUE") {
      file <- paste(file)
      utils::write.csv2(QC_scm, file = file, row.names = FALSE)
    } else {
    }
    print("successfully created data.frame 'QC_scm'")
  } else { 
    print("no false pattern in scm identifiers")
  }
}


