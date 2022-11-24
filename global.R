# options(install.packages.check.source = "no",
#         java.parameters = "-Xmx8000m")

# pckgs<-c("tidyverse", "shiny", "shinyFiles", "shinythemes", "xlsx",
#          "ggthemes", "ggplotify","xlsxjars","nortest")

pckgs<-c("tidyverse", "shiny", "shinyFiles", "shinythemes","writexl",
         "ggthemes", "ggplotify", "nortest","shinyalert","readr")

pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {
  install.packages(pckg,repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

source("functionsMarketVolumes.R")
source("functionsAbnormalReturns.R")