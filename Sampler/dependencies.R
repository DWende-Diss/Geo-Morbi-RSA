# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "checkpoint"
  ,"AMR"
  ,"data.table"
  ,"DT"
  ,"ggridges"
  ,"lubridate"
  ,"plotly"
  ,"qicharts2"
  ,"rintrojs"
  ,"shiny"
  ,"shinyBS"
  ,"shinycssloaders"
  ,"shinydashboard"
  ,"shinyjs"
  ,"shinyWidgets"
  ,"survival"
  ,"ggpubr"
  ,"survminer"
  ,"tidyverse"
  ,"viridis"
  ,"zoo"
  ,"plotly"
  ,"bit64"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

#rm(new.packages)

#library(checkpoint)
#checkpoint(snapshotDate ='2019-12-17')
library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(viridis)
library(zoo)
library(plotly)
library(bit64)



