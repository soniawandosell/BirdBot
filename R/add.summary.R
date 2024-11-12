#' Add a map command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram..
#' @param db.name Name of the database. Should be previously integred to Settings with the @backref set.database function.
#' @param description A description for the command.
#' @param today Number of days added or substracted to current date.
#' @param completed Could be "done", to show just done stuff, "to-do" or "all".
#' @param caption Title for the list. If completed="all", it should be a bector of two phrases.
#' @param mss.empty Message for when there is no content to show on the map.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.map(Settings, command="donetoday", db.name ="database", description="map of tasks done today", type="User", team=NA, today= 0, title=NA, completed="done", caption="Tasks done today", mss.empty="No tasks done today")
#'

#'@export
add.summary <- function(x=Settings,
                   command="summary",
                   db.name ="database",
                   description="list of tasks done today",
                   today= 0,
                   completed="all",
                   caption=c("Tasks done today by", "Tasks to do today"),
                   mss.empty="No tasks for today"){
  if(!db.name%in%names(x$Databases$Data)){
    stop("Database should be one of the ones included in Settings.")
  }
  if(!is.numeric(today)){
    stop("Today should be a numeric value of days added (or substracted) to todays date.")
  }

  summary.f <- function(bot= x$Configuration$bot, update){
    if(length(update$message$text)> 0L){
      if(update$message$from$id %in% x$Users$Allowed){

        date.time <- as.POSIXct(update$message$date, origin = "1970-01-01") + lubridate::days(today)
        date <- gsub(".$", "", format(date.time, "%Y-%m-%dd"))
        time <- format(date.time, "%H:%M")
        dir <- x$Databases$Directory[db.name]
        data <- read.csv(dir)%>%
          mutate(Date=as.Date(Date,format=x$Configuration$date.format))%>%as.data.frame()
        autor <- x$Users[which(x$Users$id==update$message$from$id),"name"]%>%as.character()
        title <- sub("today|tomorrow",(as.Date(date)%>% format("%d %b")),title)

        ANSWERS <- list()
        n <- length(ANSWERS)+1

        data_to_show <- data%>%select(Nests,Date,Task,User,Info,Hour)%>%filter(Date==date)
        if(completed=="done"|completed=="all"){
          done.data_to_show <- data_to_show%>%filter(!is.na(Info))
          for(user in unique(done.data_to_show$User)){
            done.u<- done.data_to_show%>%filter(User==user)
            ANSWERS[n] <-paste0(first(caption),user,":\n")
            for(row in 1:nrow(done.u)){
              ANSWERS[n] <- paste0(ANSWERS[n],
                                   paste(paste(done.u$Nest[row],done.u$User[row],done.u$Hour[row], sep=" | "),
                                         collapse = "\n"))
              if(nchar(answ1)>3500){
                n<- n+1
              }
            }



          }
        }
        if(completed=="to-do"|completed=="all"){
          todo.data_to_show <- data_to_show%>%filter(is.na(Info)&!is.na(Task))
          ANSWERS[n] <-paste0(last(caption),":\n")
          n<- n+1
          for(row in 1:nrow(done.data_to_show)){
            ANSWERS[n] <- paste0(ANSWERS[n],
                                 paste(paste(done.data_to_show$Nest[row],done.data_to_show$Task[row], sep=" | "),
                                       collapse = "\n"))
            if(nchar(answ1)>3500){
              n<- n+1
            }
          }
        }
        for(i in 1:length(ANSWERS)){
          bot$sendMessage(chat_id = update$message$chat$id, text = ANSWERS[i])
        }

        if(!completed%in%c("done","to-do","all")){
          stop("Parameter completed should be one of done, to-do or all.")
        }


        }else{
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = mss.empty)
          #bot$sendPhoto(chat_id = update$message$chat$id, photo = "empty.map.png",
          #             caption = "Empty map")
        }
      }else{
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.deny)
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.request)
      }
    }
  }
  x$Commands[[command]]<- map.f
  x$Commands.description[[command]]<- description

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]], pass_args = TRUE)
  return(x)
}
