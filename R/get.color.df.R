#' Get the colors from a dataframe.
#'
#' @param dir Directory of the excel file.
#' @param sheet. Sheet name.
#' @return A dataframe filled with colors.

get.color.df<- function(dir, sheet.){
  wb <- loadWorkbook(dir)
  df <- read.xlsx(dir, sheetName = sheet.,, check.names=F)
  styles <- sapply(getCells(getRows(getSheets(wb)[[sheet.]])), getCellStyle)
  cellColor <- function(style) {
    rgb <- paste(tryCatch(style$getFillForegroundXSSFColor()$getRgb(),
                          error = function(e) NULL), collapse = "")
    return(rgb)
  }
  color <- sapply(styles, cellColor)
  color.df <- data.frame(matrix(nrow = nrow(df), ncol = ncol(df)))
  for (i in 2:(nrow(df)+1)) {
    for (j in 1:ncol(df)) {
      color.df[i-1,j] <- ifelse(color[paste0(i,".",j)]=="",
                                NA,paste0("#",color[paste0(i,".",j)]))
    }
  }
  names(color.df) <- names(df)
  return(color.df)
}
