#' Add a sampling command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram. .
#' @param db.name Name of the database. Should be previously integred to Settings with the @backref set.database function.
#' @param description A description for the command.
#' @param sep Define the separator to write between the nestbox number and the info.
#' @param today Number of days added or substracted to current date.
#' @param type Type of data to imput. Should be one of 'Tasks', 'Status' or 'Info'.
#' @param mss.filled Message when info filled.
#' @param mss.tasks Message to show the task.
#' @param mss.rewrite Message info is already imputed.
#' @param mss.previous Message to show previous info.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.sampling(Settings, command="nest", db.name ="database", description="add info to each nest for today", sep="-", today= 0, type="Info", mss.filled=" info filled.", mss.tasks="The task for today was: ", mss.rewrite="Please use command /rewrite instead of /nest to rewrite wrong info.", mss.previous=" was previously filled as:")
#'
#' Settings <- add.sampling(Settings, command="yesterday-nest", db.name ="database", description="add info to each nest for yesterday", sep=".", today= -1, type="Info", mss.filled=" info filled.", mss.tasks="The task for yesterday was: ", mss.rewrite="Info for yesterday already imputed", mss.previous=" was previously filled as:")
#'@export
add.sampling <- function(x=Settings,
                         command="nest",
                         db.name ="database",
                         description="add info to each nest for today",
                         sep="-",
                         today= 0,
                         type="Info",
                         mss.filled=" info filled.",
                         mss.tasks="The task for today was: ",
                         mss.rewrite="Please use command /rewrite instead of /nest to rewrite wrong info.",
                         mss.previous=" was previously filled as:"){
  if(!db.name%in%names(x$Databases$Data)){
    stop("Database should be one of the ones included in Settings.")
  }
  if(!is.numeric(today)){
    stop("Today should be a numeric value of days added (or substracted) to todays date.")
  }
  if(type!="Tasks"&type!="Status"&type!="Info"){
    stop("Type should be one of 'Tasks', 'Status' or 'Info'.")
  }
  sampling.f <- function(bot=x$Configuration$bot,update,response){
    if (length(response > 0L)){
      nest.number <- unlist(strsplit(response,sep))[1]
      information <- gsub(paste0(nest.number, sep),"",response)
      if(nest.number %in% x$Locations$Nests$nests.id){
        if(update$message$from$id %in% x$Users$Allowed){
          date.time <- as.POSIXct(update$message$date, origin = "1970-01-01") + lubridate::days(today)
          date <- gsub(".$", "", format(date.time, "%Y-%m-%dd"))
          time <- format(date.time, "%H:%M")
          dir <- x$Databases$Directory[db.name]
          data <- read.csv(dir)%>%
            mutate(Date=as.Date(Date,format=x$Configuration$date.format))%>%as.data.frame()
          autor <- x$Users[which(x$Users$id==update$message$from$id),"name"]%>%as.character()
          nest.today <- data[which(data$Nests == nest.number & data$Date==date),]
          if(is.na(nest.today[,type])){
            if(type=="Tasks"&!information%in%x$Content$Tasks){
              x$Content$Tasks.df <- rbind(x$Content$Tasks.df,c(information,"Task",information,randomColor(1)))
              x$Content$Tasks <- x$Content$Tasks.df[which(x$Content$Tasks.df$type!="Status"),]$name
              wb <- loadWorkbook(x$Configuration$file)
              colors.df <- BirdBot:::get.color.df(x$Configuration$file, "Tasks")
              x$Content$Tasks.df$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                                 x$Content$Tasks.df$color)
              for (i in 1:nrow(x$Content$Tasks.df)){
                color. <- ifelse(is.na(x$Content$Tasks.df$color[i]), randomColor(1),
                                 x$Content$Tasks.df$color[i])
                style. <- CellStyle(wb)+ Fill(color.,color.,
                                              "SOLID_FOREGROUND")
                CB.setColData(CellBlock(getSheets(wb)$Tasks,
                                        startRow = i+1, startColumn=which(names(colors.df)=="color"),
                                        1, 1, create = TRUE),
                              color., colIndex= 1, rowOffset = 0,
                              showNA = F, colStyle = style.)


              }
              saveWorkbook(wb, file=x$Configuration$file)
            }
            if(type=="Status"&!information%in%x$Content$Status){
              x$Content$Tasks.df <- rbind(x$Content$Tasks.df,c(information,"Status",information,randomColor(1)))
              x$Content$Status <- x$Content$Tasks.df[which(x$Content$Tasks.df$type=="Status"),]$name
              wb <- loadWorkbook(x$Configuration$file)
              colors.df <- BirdBot:::get.color.df(x$Configuration$file, "Tasks")
              x$Content$Tasks.df$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                                 x$Content$Tasks.df$color)
              for (i in 1:nrow(x$Content$Tasks.df)){
                color. <- ifelse(is.na(x$Content$Tasks.df$color[i]), randomColor(1),
                                 x$Content$Tasks.df$color[i])
                style. <- CellStyle(wb)+ Fill(color.,color.,
                                              "SOLID_FOREGROUND")
                CB.setColData(CellBlock(getSheets(wb)$Tasks,
                                        startRow = i+1, startColumn=which(names(colors.df)=="color"),
                                        1, 1, create = TRUE),
                              color., colIndex= 1, rowOffset = 0,
                              showNA = F, colStyle = style.)


              }
              saveWorkbook(wb, file=x$Configuration$file)
            }
            data[which(data$Nests == nest.number & data$Date==date),type] <- information
            answ <- paste0(nest.number, mss.filled)
            bot$sendMessage(chat_id = update$message$chat$id, text = answ)
            if(type=="Info"){
                data[which(data$Nests == nest.number & data$Date==date),"Hour"] <- time
                data[which(data$Nests == nest.number & data$Date==date),"User"] <- autor
                if(!is.na(nest.today$Tasks)){
                  answ2 <- paste0(mss.tasks, nest.today$Tasks, ".")
                  bot$sendMessage(chat_id = update$message$chat$id, text = answ2)
                }
              }
            write.csv(data,file = x$Databases$Directory[db.name],row.names = F)
            x$Databases$Data[[db.name]] <- data
          }else{
            answ2 <- paste0(nest.number,mss.previous)
            answ3 <- nest.today[,type]
            bot$sendMessage(chat_id = update$message$chat$id, text = mss.rewrite)
            bot$sendMessage(chat_id = update$message$chat$id, text = answ2)
            bot$sendMessage(chat_id = update$message$chat$id, text = answ3)
          }
        }else{
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.deny)
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.request)
        }
      }else{
        bot$sendMessage(chat_id = update$message$chat$id, text = x$Messages$mss.wrong.nest)
      }
    }else{
      bot$sendMessage(chat_id = update$message$chat$id,
                      text =  x$Messages$mss.no.nest)
    }
  }

  x$Interaction$Commands[[command]]<- sampling.f
  x$Interaction$Commands.description[[command]]<- description

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]], pass_args = TRUE)
  return(x)
}
