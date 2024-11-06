#' Turns the database file into a calendar format excel file. It makes editing easier. Make sure you sve changes on the database with the 'calendar.to.database' function.
#'
#' @param x A 'BirdBotSettings' object.
#' @param db.name Name of the database. Should be previously integred to Settings with the @backref set.database function.
#' @backref calendar.to.database
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- database.to.calendar(Settings, db.name = "database")
#'
#'@export
database.to.calendar <- function(x=Settings,
                                 db.name = "database"){
  options(xlsx.date.format=x$Configuration$date.format)
  dir <- x$Databases$Directory[db.name]
  data.old <- read.csv(dir)%>%mutate(Date=(as.Date(Date,
                                                  format=x$Configuration$date.format)%>%
                                       format("%d.%b%y"))%>%as.character())
  if (any(is.na(data.old$Date))){
    data.old <- read.csv(dir)%>%mutate(Date=(as.Date(Date)%>%format("%d.%b%y"))%>%
                                         as.character())
  }
  data2 <- data.old%>%mutate(value = coalesce(Info,Tasks,Status),
                             Color = x$Content$Tasks.df$color[match(data.old$Status,
                                                                    x$Content$Tasks.df$name)])
  other.col <- names(data2)[!names(data2)%in%
                              c("Nests","Date","Hour","User","Status",
                                "Tasks","Info","value","Color")]
  colors <- data2 %>%select(Nests, Date, Color,all_of(other.col)) %>%
    pivot_wider(names_from = Date, values_from = Color)

  data.new <- data2 %>% select(Nests, Date, value, other.col) %>%
    pivot_wider(names_from = Date, values_from = value)%>%as.data.frame()
  date.cols <-names(data.new)[!is.na(as.Date(names(data.new), format = "%d.%b%y"))]

  wb <- createWorkbook()
  # Calendario
  addDataFrame(x = data.new, sheet = createSheet(wb, sheetName = "Calendar"),
               startCol = 1, startRow = 1, row.names = F,col.names = T,
               colnamesStyle = CellStyle(wb, font = Font(wb,isBold = T)))
  for (i in 1:nrow(data.new)) {
    for (j in which(names(data.new)%in%date.cols)) {
      color. <- colors[i, j]%>%as.character()
      if (!is.na(color.)&color.!="list(NA)") {
        style. <- CellStyle(wb)+ Fill(color.,color.,
                                      "SOLID_FOREGROUND")
        CB.setColData(CellBlock(getSheets(wb)$Calendar,
                                startRow = i+1, startColumn=j,
                                1, 1, create = TRUE),
                      data.new[i,j], colIndex= 1, rowOffset = 0,
                      showNA = F, colStyle = style.)
      }
    }
  }

  # Leyenda
  legend.df <- x$Content$Tasks.df%>%arrange(type)
  addDataFrame(x = legend.df, sheet = createSheet(wb, sheetName = "Legend"),
               startCol = 1, startRow = 1, row.names = F,
               colnamesStyle = CellStyle(wb,
                                         font = Font(wb,isBold = T)))
  for (i in 1:nrow(legend.df)) {
    color. <- ifelse(is.na(legend.df$color[i]), randomColor(1),
                     legend.df$color[i])
    style. <- CellStyle(wb)+ Fill(color.,color.,
                                  "SOLID_FOREGROUND")
    CB.setColData(CellBlock(getSheets(wb)$Legend,
                            startRow = i+1, startColumn=which(names(legend.df)=="color"),
                            1, 1, create = TRUE),
                  color., colIndex= 1, rowOffset = 0,
                  showNA = F, colStyle = style.)
  }

  x$Databases$Calendar[[db.name]] <- wb
  if(any(duplicated(filter(x$Content$Tasks.df,type=="Status")[,"color"]))){
    stop("You need to choose one different color for each status type on the Settings file.")
  }
  saveWorkbook(wb, paste0(file_path_sans_ext(sub(db.name,paste0("Calendar.",
                                                                db.name),
                                                 dir)),".xlsx"))
  return(x)
}
