sapply(c("data.table", "ggplot2", "stringi"), require, character.only = TRUE)

# feature engineering of aggregated data
aggr_2014 <-  read.csv("aggr_2014.csv")
aggr_2014$X <- NULL


flag_to_cat <- function(df , firstcol, parentlength) {
  ratio <- 0.5
  newcol <- rep("x", nrow(df))
  
  #calculating variables' length:
  cola <- head(which(grepl(substr(firstcol, 0, 2) , sapply(names(df), substr, 0, 2)) == TRUE), 1)
  colb <- tail(which(grepl(substr(firstcol, 0, 2) , sapply(names(df), substr, 0, 2)) == TRUE), 1)
  
  for (i in 1:nrow(df)) {
    max <- max(df[i, cola:colb])
    second <- sort(df[i, cola:colb],partial = colb - cola)[colb - cola][1,1]
    ifelse(second * (1 + ratio) < max, 
           val <- names(which(apply(df[i, cola:colb] == max, 2, any))), 
           val <- paste0(stri_dup("x", parentlength),"EVEN"))
    newcol[i] <- substr(x = val, start = parentlength + 1, stop = nchar(val))
  }
  
  return(newcol)
}

aggr_2014$MAX_CHANNEL <- factor(flag_to_cat(aggr_2014, "CHANNELb", 7))
aggr_2014$MAX_TIME_CAT <- factor(flag_to_cat(aggr_2014, "TIME_CAT.", 8))
aggr_2014$MAX_LOC_CAT <- factor(flag_to_cat(aggr_2014, "LOC_CAT.", 7))
aggr_2014$MAX_MC_CAT <- factor(flag_to_cat(aggr_2014, "MC_CAT.", 6))
aggr_2014$MAX_CARD_CAT <- factor(flag_to_cat(aggr_2014, "CARD_CAT.", 8))
aggr_2014$MAX_AMT_CAT <- factor(flag_to_cat(aggr_2014, "AMT_CAT.", 7))
aggr_2014$MAX_WD <- factor(flag_to_cat(aggr_2014, "wdhétfő", 2))
aggr_2014$MAX_MONTH <- factor(flag_to_cat(aggr_2014, "MONTH_CAT.", 9))

aggr_2014 <- aggr_2014[-c(2:43)]

summary(aggr_2014)

  
