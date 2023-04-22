# set directory
library(tidyverse)
setwd("[insert path]")
outdir <- "./output/"
datadir <- "./data"
codedir <- "./code"
variable_labels <- read.csv(paste0(codedir, "/variable labels.csv"), header=T)

#-----Set up for 'variable_selection.R'-----
# values: {"EMA","Passive","EMA and Passive"}
source <- "Passive"

rhs.day <- c("Day","DayOfWeek")
rhs.vars.ema <- c("[insert]")
rhs.vars.pass <- c("[insert]")
rhs.vars.emapass <- c(rhs.vars.ema,rhs.vars.pass)

if (source == "EMA") {
  rhs.vars <- c(rhs.day,rhs.vars.ema)
  missing_var <- "Missingness"
  srce <- "EMA"
}
if (source == "Passive") {
  rhs.vars <- c(rhs.day,rhs.vars.pass) 
  missing_var <- "Adherence" # adherence for wearing Fitbit device
  srce <- "Pass"
}
if (source == "EMA and Passive") {
  rhs.vars <- c(rhs.day,rhs.vars.emapass)
  missing_var <- c("Missingness","Adherence")
  srce <- "EMAPass"
}

resp.vars <- "SI_any_nextday"
id <- "ID"

all.vars <- c(id,resp.vars,rhs.vars)

rhs.vars.add <- paste(rhs.vars,collapse = " + ")
formula_PGEE <- paste(resp.vars," ~ ",rhs.vars.add) 

#-----Set up for 'prediction.R'-----
with_SI = "with SI"
rhs.vars.PGEE.ema <- c("[insert]")

with_SI = "without SI"
rhs.vars.PGEE.ema <- c("[insert]")

rhs.vars.PGEE.pass <- c("[insert]")

if (source == "EMA") {
  rhs.vars.PGEEstandard <- rhs.vars.PGEE.ema
}
if (source == "Passive") {
  rhs.vars.PGEEstandard <- rhs.vars.PGEE.pass
  with_SI = "not applicable"
}
if (source == "EMA and Passive") {
  rhs.vars.PGEEstandard <- c(rhs.vars.PGEE.ema,rhs.vars.PGEE.pass)
}

rhs.vars.PGEE.add <- paste(rhs.vars.PGEEstandard,collapse = " + ")
formula_glmertree <- paste(resp.vars," ~ 1 | ID | ",rhs.vars.PGEE.add)

#-----Read in for 'variable_selection.R', 'prediction.R', 'main_figures.R'-----
EMA_dat <- read.csv(paste0(datadir,"[insert filename].csv"), header=T)

passive_dat <- read.csv(paste0(datadir,"[insert filename].csv"), header=T)

EMA_passive_dat <- merge(x=EMA_dat,y=passive_dat,by=c("ID","Day"), all.x=T)

analytic_dat <- EMA_passive_dat%>%
  dplyr::select(all_of(c(all.vars)))%>%
  na.omit() # complete cases

#-----Run scripts-----
source(paste0(codedir, "/utils.R"))

source(paste0(codedir, "/variable_selection.R"))

source(paste0(codedir, "/prediction.R"))

source(paste0(codedir, "/main_figures.R"))

source(paste0(codedir, "/supp_figures.R"))