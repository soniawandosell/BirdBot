
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `BirdBot`: <img src="man/figures/BirdBot.png" style="float: right; border: 0; margin: auto;" align="right" height="139"/>

## A sampling tool for data collection from nest-boxes on the field based on Telegram

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)

<!-- badges: end -->

<img src="vignettes/birdbot_poster.png"/>

## Installation

You can install the development version of BirdBot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soniawandosell/BirdBot")
```

## How to use

1.  Configure the bot.
2.  Run the bot and collect data in the field.
3.  Edit the database.

## Functions

- `set.settings()`: creates and updates a settings file.  
- `set.database()`: creates and updates a database file.  
- `add.start()`: adds a start command to the bot.  
- `add.legend()`: adds a legend command to the bot.  
- `add.newuser()`: adds a user request command to the bot.  
- `add.sampling()`: adds a sampling command to the bot.  
- `add.rewrite()`: adds a rewrite command to the bot.  
- `add.map()`: adds a map command to the bot.  
- `add.view()`: adds a chart view command to the bot.  
- `add.download()`: adds a download command to the bot.  
- `add.summary()`: adds a summary command to the bot.  
- `add.messages()`: adds a message handler to the bot.  
- `database.to.calendar()`: transforms the database into a calendar view
  excel file.  
- `calendar.to.database()`: updates the database with the calendar excel
  file.  
- `add.messages()`: adds a message handler to the bot.  
- `run.Bot()`: runs the bot.  
- `database.to.calendar()`: transforms the database into a calendar view
  excel file.  
- `calendar.to.database()`: updates the database with the calendar excel
  file.
