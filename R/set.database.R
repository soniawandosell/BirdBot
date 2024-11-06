#' Crates a database file with a row for each date and nestbox.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command A number.
#' @param db.name Name of the database.
#' @param firstday Date in which the database will start.
#' @param lastday Date in which the database will end
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- set.database(Settings, db.name = "database", firstday = "2024-10-28", lastday = "2024-11-06")
#'
#'@export
set.database <- function(x=Settings,
                         db.name = "database",
                         firstday = "2024-10-28",
                         lastday = "2024-11-06"){
  dir <- paste0(getwd(),"/data/",db.name,".csv")
  if(!file.exists(dir)){
    firstday <- as.Date(firstday)
    lastday <- as.Date(lastday)
    plots <- x$Locations$Nests$nests.id
    data <- expand.grid( Nests = plots, Date = seq(firstday, lastday, 1))
    data$Hour <- NA
    data$User <- NA
    data$Color <- NA
    data$Status <- NA
    data$Tasks <- NA
    data$Info <- NA
    data$Comments <- NA
    write.csv(data,dir, row.names = F)
  }else{
    data <- read.csv(dir)%>%mutate(Date=as.Date(Date,
                                                format=x$Configuration$date.format))
    if(lastday>max(data$Date)){
      lastday <- as.Date(lastday)
      plots <- x$Locations$Nests$nests.id
      firstday <- max(data$Date)+1
      data2 <- expand.grid( Nests = plots, Date = seq(firstday, lastday, 1))
      data <- bind_rows(data, data2)
      write.csv(data,dir, row.names = F)
      x$Databases$Data[[db.name]] <- data
    }
  }
  x$Databases$Directory[db.name] <- dir
  x$Databases$Data[[db.name]] <- data
  save(x, file = paste0(file_path_sans_ext(x$Configuration$file), ".RData"))
  return(x)
}
