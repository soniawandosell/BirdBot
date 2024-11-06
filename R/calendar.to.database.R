#' Function to refresh the database with the calendar excel file.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command A number.
#' @param db.name Name of the database. Should be previously integred to Settings with the @backref set.database function.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- calendar.to.database(Settings, db.name = "database")
#'
#'@export
calendar.to.database <- function(x=Settings,
                                 db.name = "database"){
  options(xlsx.date.format=x$Configuration$date.format)
  dir <- x$Databases$Directory[db.name]
  data.old <- x$Databases$Data[[db.name]]
  wb <- loadWorkbook(paste0(file_path_sans_ext(sub(db.name,paste0("Calendar.",
                                                                  db.name),
                                                   dir)),".xlsx"))
  data.new <- read.xlsx(paste0(file_path_sans_ext(sub(db.name,paste0("Calendar.",
                                                                     db.name),
                                                      dir)),".xlsx"),
                        sheetName = "Calendar", check.names=F)
  date.cols <-names(data.new)[!is.na(as.Date(names(data.new), format = "%d.%b%y"))]
  other.col <- names(data.new)[!names(data.new)%in%date.cols]
  colors.df<- BirdBot:::get.color.df(paste0(file_path_sans_ext(sub(db.name,paste0("Calendar.",
                                                                        db.name),
                                                         dir)),".xlsx"),"Calendar")

  colors.df[,other.col] <- data.new[,other.col]
  colors.df <- colors.df%>%
    pivot_longer(cols = date.cols, names_to = "Date", values_to = "color")

  data.new <- data.new%>%
    mutate(across(date.cols, as.character))%>%
    pivot_longer(cols = date.cols, names_to = "Date", values_to = "value")%>%
    merge(colors.df)%>%left_join(subset(x$Content$Tasks.df,type=="Status")%>%
                                   select(color, name), by = "color",
                                 relationship = "many-to-many")%>%
    mutate(Status = ifelse(value %in% x$Content$Status, value,
                           ifelse(!is.na(name),name,NA)),
           Tasks = ifelse(value %in% x$Content$Tasks, value, NA),
           Info = ifelse(is.na(Status) & is.na(Tasks), value, NA))%>%
    mutate(Hour=NA,User=NA)%>%
    select(other.col,Date,Hour,User,Status,Tasks,Info)%>%
    mutate(Date=as.Date(Date,
                        format="%d.%b%y")%>%
             format(x$Configuration$date.format),
           Info=ifelse(Info=="",NA,Info))

  # Rellenar data.new con la info previa de data.old
  for(i in 1:nrow(data.new)){
    date. <- data.new[i,"Date"]
    nest. <- data.new[i,"Nests"]
    data.old.i <- data.old[which(data.old$Date==date.& data.old$Nest==nest.),]
    if(nrow(data.old.i)>0){
      data.new[i,"Tasks"] <- ifelse(!is.na(data.new[i,"Info"])&!is.na(data.old.i$Info),
                                   data.old.i$Tasks, data.new[i,"Tasks"])
      data.new[i,"Hour"] <- ifelse(!is.na(data.new[i,"Info"])&!is.na(data.old.i$Info),
                                   data.old.i$Hour, data.new[i,"Hour"])
      data.new[i,"User"] <- ifelse(!is.na(data.new[i,"Info"])&!is.na(data.old.i$Info),
                                   data.old.i$User, data.new[i,"User"])
    }
  }

  # Leyenda
  x$Content$Tasks.df <- read.xlsx(paste0(
    file_path_sans_ext(sub(db.name,paste0("Calendar.",db.name),dir)),".xlsx"),
    sheetName = "Legend")
  x$Content$Tasks <- x$Content$Tasks.df[which(x$Content$Tasks.df$type!="Status"),]$name
  x$Content$Status <- x$Content$Tasks.df[which(x$Content$Tasks.df$type=="Status"),]$name

  wb.l <- loadWorkbook(x$Configuration$file)
  colors.df <- BirdBot:::get.color.df(x$Configuration$file, "Tasks")
  x$Content$Tasks.df$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                     x$Content$Tasks.df$color)
  for (i in 1:nrow(x$Content$Tasks.df)) {
    color. <- ifelse(is.na(x$Content$Tasks.df$color[i]), randomColor(1),
                     x$Content$Tasks.df$color[i])
    style. <- CellStyle(wb.l)+ Fill(color.,color.,
                                  "SOLID_FOREGROUND")
    CB.setColData(CellBlock(getSheets(wb.l)$Tasks,
                            startRow = i+1, startColumn=which(names(colors.df)=="color"),
                            1, 1, create = TRUE),
                  color., colIndex= 1, rowOffset = 0,
                  showNA = F, colStyle = style.)
  };saveWorkbook(wb.l, file=x$Configuration$file)


  x$Databases$Data[[db.name]] <- data.new
  save(x, file = paste0(basename(file_path_sans_ext(x$Configuration$file)), ".RData"))
  write.csv(data.new,dir, row.names = F)
  return(x)
}
