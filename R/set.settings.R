#' Sets Settings for the Bot. It will create a Settings file or refresh an existing one.
#'
#' @param dir A 'BirdBotSettings' object.
#' @param date.format The date format used on excel files.
#' @param map.file Directory for the '.klm' map file. Could be NA if you do not have a file.
#' @param mss.deny Message to deny access to the bot.
#' @param mss.request Message to request for access to the bot.
#' @param mss.wrong.nest Message if incorrect nestbox number is typed.
#' @param mss.no.nest Message if no nestbox number is typed.
#' @param mss.unknown Ununderstood message.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- set.settings(dir ="./data/Settings.xlsx", date.format="%d/%m/%Y", map.file="./data/StudyArea .kml")
#'
#'@export
set.settings <- function(dir ="./data/Settings.xlsx",
                         date.format="%d/%m/%Y",
                         map.file="./data/StudyArea.kml",
                         mss.deny="You are not allowed to use this bot. \ud83d\udeab",
                         mss.request ="Please request access by sending a \"/newuser\" message.",
                         mss.wrong.nest="Please, enter a valid nestbox number.",
                         mss.no.nest="Please, enter a nestbox number.",
                         mss.unknown ="I did not understand. \ud83d\ude30 Please, repeat."){
  if(!file.exists(dir)){
    token.df <- data.frame(token="your telegram token here")%>%as.data.frame()
    users.df <- data.frame(chat.id=NA,
                           permit=NA,
                           name=NA,
                           alias=NA,
                           color=NA)
    tasks.df <- data.frame(name=NA,
                           type=NA,
                           description=NA,
                           color=NA)
    nests.df <- data.frame(plot.id=NA,
                           x=NA,
                           y=NA,
                           color=NA)
    labels.df <- data.frame(label=NA,
                            x=NA,
                            y=NA,
                            color=NA,
                            size=NA)
    segments.df <- data.frame(description=NA,
                              x1=NA,
                              x2=NA,
                              y1=NA,
                              y2=NA,
                              colour=NA,
                              size=NA)
    sheets <- c("Token","Users","Tasks","Nests","Map.labels","Map.segments")
    dfs <- list(token.df,users.df,tasks.df,nests.df,labels.df,segments.df)
    wb <- createWorkbook()
    for (i in 1:6){
      sheet <- createSheet(wb, sheetName = sheets[i])
      addDataFrame(x = dfs[[i]], sheet = sheet,
                startCol = 1, startRow = 1, row.names = F,
                colnamesStyle = CellStyle(wb,
                  font = Font(wb,isBold = T)))
    }
    saveWorkbook(wb, file=dir)
    cat("Enter your data in the Settings file created and re-run this function.\n")

  }else{
    if(!file.exists(paste0(basename(file_path_sans_ext(dir)), ".RData"))){
      # Creamos BirdBotSettings
      x <- list()
      x$Configuration <- list()
      x$Users <- list()
      x$Content <- list()
      x$Databases <- list()
      x$Locations <- list()
      x$Interaction <- list()

      #Databases
      x$Databases$Directory <- c()
      x$Databases$Data <- list()
      x$Databases$Calendar <- list()

      #Interaction
      x$Interaction$Commands <- list()
      x$Interaction$Descriptions <- list()

      #Messages
      x$Messages <- list()

      class(x) <- "BirdBotSettings"
    }
    wb <- loadWorkbook(dir)
    #Configuration
    x$Configuration$token <- read.xlsx(dir, sheetName = "Token")[1,1]
    x$Configuration$bot <- Bot(token = x$Configuration$token)
    x$Configuration$updater <- Updater(token = x$Configuration$token)
    x$Configuration$file <- dir
    x$Configuration$date.format <- date.format
    if(length(x$Configuration$token)!=1){
      stop("Wrong token format. Make sure there is a token on Settings file")
    }

    #Users
    x$Users$Users <- read.xlsx(dir,sheetName = "Users")
    x$Users$Allowed <- subset(x$Users$Users,
                              permit =="user"|permit =="admin")$chat.id
    x$Users$Admin <- subset(x$Users$Users,
                            permit =="admin")$chat.id
    x$Users$To_allow <- subset(x$Users$Users,
                               permit =="to.allow")$chat.id
    colors.df <- BirdBot:::get.color.df(dir, "Users")
    x$Users$Users$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                 x$Users$Users$color)
    for (i in 1:nrow(x$Users$Users)) {
        color. <- ifelse(is.na(x$Users$Users$color[i]), randomColor(1),
                         x$Users$Users$color[i])
        style. <- CellStyle(wb)+ Fill(color.,color.,
                                      "SOLID_FOREGROUND")
        CB.setColData(CellBlock(getSheets(wb)$Users,
                                startRow = i+1, startColumn=which(names(colors.df)=="color"),
                                1, 1, create = TRUE),
                      color., colIndex= 1, rowOffset = 0,
                      showNA = F, colStyle = style.)
    }
    if(!any(unique(x$Users$Users$permit)%in%c("user","admin","to.allow"))){
      stop("Users permit just can be 'user', 'admin' or 'to.allow'.")
    }

    #Content
    x$Content$Tasks.df <- read.xlsx(dir,sheetName = "Tasks")
    x$Content$Tasks <- x$Content$Tasks.df[which(x$Content$Tasks.df$type!="Status"),]$name
    x$Content$Status <- x$Content$Tasks.df[which(x$Content$Tasks.df$type=="Status"),]$name
    wb <- loadWorkbook(dir)
    colors.df <- BirdBot:::get.color.df(dir, "Tasks")
    x$Content$Tasks.df$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                       x$Content$Tasks.df$color)
    for (i in 1:nrow(x$Content$Tasks.df)) {
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

    #Locations
    if(!is.na(map.file)){
      map.sf <- st_read(map.file)
      # Nests
      puntos_sf <- map.sf[st_geometry_type(map.sf) == "POINT", ]
      nests.df <- puntos_sf %>%
        st_coordinates() %>% as.data.frame() %>%
        mutate(nests.id=puntos_sf$Name, color="#000000", size=1)%>%
        select(nests.id,x = X,y = Y,color,size)
      x$Locations$Nests <- nests.df

      # Segments
      lines_sf <- map.sf %>% filter(st_geometry_type(.) %in%
                                      c("POLYGON", "MULTIPOLYGON", "LINESTRING"))
      segments.df <- data.frame(description=NA,x1=NA,y1=NA,x2=NA,y2=NA, color=NA, size=NA)
      r <- 1
      for (t in 1:nrow(lines_sf)) {
        polygon <- (lines_sf$geometry[t])%>%st_coordinates()%>%as.data.frame()
        name <- lines_sf$Name[t]
        for (s in 2:nrow(polygon)) {
          x1 <- polygon$X[s-1]
          x2 <- polygon$X[s]
          y1 <- polygon$Y[s-1]
          y2 <- polygon$Y[s]
          segments.df.s <- c(name, x1,y1,x2,y2,"#555555",.5)
          segments.df[r,] <- segments.df.s
          r <- r+1
        }
      }
      x$Locations$Segments <- segments.df%>%mutate(x1=as.numeric(x1),
                                                   x2=as.numeric(x2),
                                                   y1=as.numeric(y1),
                                                   y2=as.numeric(y2),
                                                   size=as.numeric(size))
      # Labels
      labels.df <- x$Locations$Segments %>% group_by(description) %>% filter(n() > 1) %>%
        mutate(x = (x1+x2)/2, y = (y1+y2)/2) %>%
        rename(label= description)%>%
        summarise( x = mean(x), y = mean(y)) %>% as.data.frame()%>%
        mutate(color="#333333", size=3)
      x$Locations$Labels <- labels.df

      sheet. <- c("Nests","Map.labels","Map.segments")
      df. <- list(x$Locations$Nests,x$Locations$Labels,x$Locations$Segments)

      for (o in 1:3) {
        removeRow(getSheets(wb)[[sheet.[o]]],
                  getRows(getSheets(wb)[[sheet.[o]]]))
        addDataFrame(x = df.[o], sheet = getSheets(wb)[[sheet.[o]]],
                     startCol = 1, startRow = 1, row.names = F,
                     colnamesStyle = CellStyle(wb,
                                               font = Font(wb,isBold = T)))
        for (i in 1:nrow(df.[[o]])) {
          color. <- df.[[o]]$color[i]
          style. <- CellStyle(wb)+ Fill(color.,color.,
                                        "SOLID_FOREGROUND")
          CB.setColData(CellBlock(getSheets(wb)[[sheet.[o]]],
                                  startRow = i+1, startColumn=which(names(df.[[o]])=="color"),
                                  1, 1, create = TRUE),
                        color., colIndex= 1, rowOffset = 0,
                        showNA = F, colStyle = style.)
        }
      }

    }else{
      x$Locations$Nests <- read.xlsx(dir,sheetName = "Nests")
      x$Locations$Labels <- read.xlsx(dir,sheetName = "Map.labels")
      x$Locations$Segments <- read.xlsx(dir,sheetName = "Map.segments")
      colors.df <- BirdBot:::get.color.df(dir, "Nests")
      x$Locations$Nests$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                        x$Locations$Nests$color)
      for (i in 1:nrow(x$Locations$Nests)) {
        color. <- ifelse(is.na(x$Locations$Nests$color[i]), randomColor(1),
                         x$Locations$Nests$color[i])
        style. <- CellStyle(wb)+ Fill(color.,color.,
                                      "SOLID_FOREGROUND")
        CB.setColData(CellBlock(getSheets(wb)$Nests,
                                startRow = i+1, startColumn=which(names(colors.df)=="color"),
                                1, 1, create = TRUE),
                      color., colIndex= 1, rowOffset = 0,
                      showNA = F, colStyle = style.)
      }
      if(nrow(x$Locations$Labels)>0){
        colors.df <- BirdBot:::get.color.df(dir, "Map.labels")
        x$Locations$Labels$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                           x$Locations$Labels$color)
        for (i in 1:nrow(x$Locations$Labels)) {
          color. <- ifelse(is.na(x$Locations$Labels$color[i]), randomColor(1),
                           x$Locations$Labels$color[i])
          style. <- CellStyle(wb)+ Fill(color.,color.,
                                        "SOLID_FOREGROUND")
          CB.setColData(CellBlock(getSheets(wb)$Map.labels,
                                  startRow = i+1, startColumn=which(names(colors.df)=="color"),
                                  1, 1, create = TRUE),
                        color., colIndex= 1, rowOffset = 0,
                        showNA = F, colStyle = style.)
        }
      }
      if(nrow(x$Locations$Segments)>0){
        colors.df <- BirdBot:::get.color.df(dir, "Map.segments")
        x$Locations$Segments$color <- ifelse(!is.na(colors.df$color),colors.df$color,
                                             x$Locations$Segments$color)
        for (i in 1:nrow(x$Locations$Segments)) {
          color. <- ifelse(is.na(x$Locations$Segments$color[i]), randomColor(1),
                           x$Locations$Segments$color[i])
          style. <- CellStyle(wb)+ Fill(color.,color.,
                                        "SOLID_FOREGROUND")
          CB.setColData(CellBlock(getSheets(wb)$Map.segments,
                                  startRow = i+1, startColumn=which(names(colors.df)=="color"),
                                  1, 1, create = TRUE),
                        color., colIndex= 1, rowOffset = 0,
                        showNA = F, colStyle = style.)
        }
      }
    }
    #Messages
    x$Messages$mss.deny <- mss.deny
    x$Messages$mss.request <- mss.request
    x$Messages$mss.wrong.nest <- mss.wrong.nest
    x$Messages$mss.no.nest <- mss.no.nest
    x$Messages$mss.unknown <- mss.unknown

    saveWorkbook(wb, file=dir)
    #save(x, file = paste0(getwd(),"/",basename(file_path_sans_ext(dir)), ".RData"),
     #    envir = environment())
    save(x, file = paste0((file_path_sans_ext(dir)), ".RData"))
    return(x)
  }
}
