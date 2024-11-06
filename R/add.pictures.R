#' Add a messages handler to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param image.folder Folder to save pictures.
#' @param mss.saved Message shown when picture saved.
#' @param mss.caption Requests a caption to title the picture with.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.sampling(Settings, pic.folder="./Images/", mss.saved.pic="Picture saved!", mss.caption.pic="Please send the picture again with a caption.")
#'
#'@export
add.messages <- function(x=Settings,
                         pic.folder="./Images/",
                         mss.saved.pic="Picture saved!",
                         mss.caption.pic="Please send the picture again with a caption."){
  if(!db.name%in%names(x$Databases$Data)){
    stop("Database should be one of the ones included in Settings.")
  }
  messages.f <- function(bot=x$Configuration$bot,update){
    if(length(update$message$photo[[1L]]$file_id> 0L)){
        if(update$message$from$id %in% x$Users$Allowed){
          date.time <- as.POSIXct(update$message$date, origin = "1970-01-01") + lubridate::days(today)
          date <- gsub(".$", "", format(date.time, "%Y-%m-%dd"))
          time <- format(date.time, "%H:%M")
          caption <- update$message$caption
          data <- read.csv(dir)%>%
            mutate(Date=as.Date(Date,format=x$Configuration$date.format))%>%as.data.frame()
          autor <- x$Users[which(x$Users$id==update$message$from$id),"name"]%>%as.character()
          nest.today <- data[which(data$Nests == nest.number & data$Date==date),]
          if(length(caption)>0){
            bot$getFile(file_id = update$message$photo[[4L]]$file_id,
                        destfile = paste0(pic.folder,
                                          date,".",caption,".jpg"))
            bot$sendMessage(chat_id = update$message$chat$id,
                            text = mss.saved.pic)
          }else{
            bot$sendMessage(chat_id = update$message$chat$id,
                            text = mss.caption.pic)
          }
        }else{
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.deny)
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.request)
        }
    }
    if(length(update$message$text> 0L)){
      if(update$message$from$id %in% x$Users$Allowed){
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.unknown)
      }else{
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.deny)
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.request)
      }
    }
  }
  x$Commands[["messages"]]<- messages.f
  x$Commands.description[["messages"]]<- "function to save pictures and handle unknown messages"

  x$Configuration$updater <- x$Configuration$updater +
    MessageHandler(x$Interaction$Commands[["messages"]])

  return(x)
}
