#### Loading packages ######


p_needed <-
  c("dplyr","ggplot2","stringr","rlist","tools",
    "tidyr","rJava","leaflet","sf",
    "rmapshaper", "shiny","DT",
    "rgdal","RSelenium",
    "rvest","httr","stringr","rlist","tools",
    "tidyr", "XML","rJava")


packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)