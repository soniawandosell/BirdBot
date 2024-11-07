# Bibliotecas ----
{
  library(telegram.bot)
  library(readr);library(xlsx)
  library(anytime);library(lubridate)
  library(stringr)
  library(magrittr)
  #library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(ggpmisc);library(grid);library(gridExtra)
  library(usethis)
  library(randomcoloR)
  library(tools)
  library(sf)
  library(devtools);library(roxygen2);library(pkgdown)
  # library(xml2)
  # load("C:/Users/CBM/Dropbox/Personal/Databot/.RData")
}
# Documentación
roxygen2::roxygenise()
use_vignette("introduction")
devtools::document()
use_pkgdown_github_pages()
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_testthat()
devtools::check()
devtools::build()


# Actualizar paquete
system("git add .")
system("git commit -m 'First steps'")
system("git push origin main")
system("git tag -a v1.0.0 -m 'Versión 1.0.0'")
system("git push origin v0.0.0")
usethis::use_pkgdown_github_pages()

# Package functions -----
file.edit("R/database.to.calendar.R")
file.edit("R/calendar.to.database.R")#
file.edit("R/set.database.R")
file.edit("R/set.settings.R")#
file.edit("R/get.color.df.R")
file.edit("R/run.Bot.R")

file.edit("R/add.newuser.R")
file.edit("R/add.start.R")
file.edit("R/add.legend.R")
file.edit("R/add.sampling.R")
file.edit("R/add.map.R")
file.edit("R/add.rewrite.R")
file.edit("R/add.view.R")
file.edit("R/add.messages.R")
file.edit("R/add.download.R")

#---------------------------------

library(qrcode)
qr <- qr_code("https://github.com/soniawandosell/BirdBot")
plot(qr)
edit_git_config()
use_git()

# Bibliotecas ----
{
  library(telegram.bot)
  library(readr);library(xlsx)
  library(anytime);library(lubridate)
  library(stringr)
  library(magrittr)
  library(tidyverse)
  library(ggpmisc);library(grid);library(gridExtra)
  library(usethis)
  library(randomcoloR)
  library(tools)
  library(sf)
  library(roxygen2)
  load("C:/Users/CBM/Dropbox/Personal/Databot/.RData")
}
save.image(".RData")


wb <- loadWorkbook(dir)
bot$sendPhoto(chat_id = 988967855, photo = img.name,
              caption = paste0("Last days of nestbox ", nest.number))

#---------------------------------

library(qrcode)
qr <- qr_code("https://github.com/soniawandosell")
plot(qr)
edit_git_config()
use_git()

#
#Package functions:
#_____________________
#IDEAS:
#
#Color sólo en settings, asociado a status, leyenda refresque settings content
#Comentarios en calendario
#Que en calendar.to.database se priorice si cell content está en stasus,
#    (por si el color es confuso)
#Dar opción de Renviron en vez de Token
#stop messages
#editar examples de todas las funciones
#cambiar nombre a BirdBot (también funciones dentro del script)
#_____________________
#HECHO:
#
#  set.settings
#create.database
#database.to.calendar
#add.newuser.f
#add.start.f
#add.legend.f
#add.sampling.f
#maps
#teams
#mover mensajes base a settings
#
#Calendar to database:
#
#  status actualizar con colores
#Tasks mantener si antes había info y ahora también hay info
#Cambiar si hay nueva task
#Mejor hacer un bucle fila por fila.
#Si antes no había un día ampliar fecha-cajanido.
#Si hemos hecho una columna nueva en calendario dejarla
#Estaría bien que en colores oscuros aparezca la letra en blanco?
#  Mejorar mensajes, elementos para sustituir
#Añadir leyenda al calendario
#Si hay un color en settings$content$tasks.df hacer que se escriba y prevalece el color con respecto al texto
#
#Set.settings acttualice settings, no sólo lo cree
#
#
