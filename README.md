
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BirdBot

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

A fundamental concern in contemporary research is the safeguarding of
empirical data and transparency in the data acquisition process.
Traditional (paper and pencil) field data collection is subject to many
limitations and open to accidental loss. For example, changing weather
conditions or the inherent difficulties in coordinating large working
groups can hamper data reliability. Mobile devices are now almost
universally accessible, and the development of data logging applications
is booming. These new applications could optimise work in the field by
providing security, ease of communication and coordination between users
in a team. Currently, many research groups involved in bird ecology
studies store their databases in Excel files. It is possible to design a
mobile application for data collection that facilitates the flow of
information from the mobile device to the final Excel file, eliminating
the intermediate step of transcribing data from the field notebook to
the computer. In order to solve the problems attributed to traditional
methods of field data collection we have developed a configurable tool
programmed in R language:
[BirdBot](https://github.com/soniawandosell/BirdBot). This application
connects the storage of nest box records (or geo-located natural nests)
in a remote Excel database via a Telegram chat based on simple
command-based communication. By logging data via chat, BirdBot provides
a backup of the data stored on each of the mobile devices. This prevents
any loss of information. Another advantage over traditional methods is
that user name, date and time data is stored without the need to be
typed in. [BirdBot](https://github.com/soniawandosell/BirdBot) offers
the possibility to create topological maps from the mobile phone with
information about pending and completed tasks as well as the status of
nest boxes. It is also possible to create maps showing the boxes checked
by each user, informing about the last nest box visited by each team
member. This is an important logistical advantage, as it increases the
efficiency of fieldwork and improves safety in the field by recording
the route taken by each team member. In conclusion, the aim of Birdbot
is to provide free software for research teams working with breeding
birds in nest boxes or natural nests, facilitating fieldwork and the
safeguarding of the data obtained.

## 

## Installation

You can install the development version of BirdBot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soniawandosell/BirdBot")
```
