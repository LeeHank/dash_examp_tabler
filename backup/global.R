library(shiny)
library(shinymanager)
library(httr)
library(bslib)
library(tidyverse)
library(plotly)
library(reactable)
library(waiter)
library(shinycssloaders)
library(lubridate)
library(ggrepel)
library(readxl)
library(gridExtra)
# library(bs4Dash)
# library(echarts4r)
source("R/utils.R")
source("R/custom_bs4_widgets.R")

set_labels(
  language = "en",
  "Please authenticate" = "AV(%) Dashboard",
  "Username:" = "EMPNO (工號)",
  "Password:" = "Password (筆電密碼)"
)

data_current_proc = readRDS("./data/data_current_proc.rds")
data_hist_summary = readRDS("./data/data_hist_summary.rds")
card_id_info = readRDS("./data/card_id_info.rds")
card_param = readRDS("./data/card_param.rds")




av_criteria = read_excel("./data/av_criteria.xlsx") %>%
  mutate(across(c(CEO_AV_CRIT:BU_HEAD_AV_CRIT), ~.x/100))
av_criteria_for_plot = av_criteria %>%
  pivot_longer(cols = c(CEO_AV_CRIT, BG_HEAD_AV_CRIT, BU_HEAD_AV_CRIT),
               names_to = "ROLE",
               values_to = "AV_PREC") %>%
  mutate(AV_PREC = AV_PREC/100)