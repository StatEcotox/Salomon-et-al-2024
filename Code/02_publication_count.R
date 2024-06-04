# Load packages
library(dplyr)

# Import data
analysis_dat = readRDS("Data/analysis_dat.rds")

str(analysis_dat)

## Total---
dim(analysis_dat) #305 samples #7 variables
length(unique(analysis_dat$pub_ID)) #13 publications



###Year--------
analysis_dat$year = as.factor(analysis_dat$year)
year <- levels(analysis_dat$year)
year #"2016" "2018" "2019" "2020" "2021" "2022"



####Conditioned-----
analysis_dat$DOM_conditioned = as.factor(analysis_dat$DOM_conditioned)
dom_conditioned <- levels(analysis_dat$DOM_conditioned)
dom_conditioned

n.studies= data.frame(
  DOM_conditioned =  dom_conditioned,
  n.stud = rep(NA,length(dom_conditioned))
)

i=1
for(p in dom_conditioned){
  sub = analysis_dat$pub_ID[analysis_dat$DOM_conditioned == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i+1
} 

n.studies

# DOM_conditioned n.stud
# 1              no      6
# 2             yes      7


# number of data points:
analysis_dat %>% group_by(DOM_conditioned) %>% summarize(n = n())

# DOM_conditioned     n
# 1 no                200
# 2 yes               105


####Exposure concentration-----
summary(analysis_dat$DOM_conc_mg_L) 


####DOM type-----
analysis_dat$DOM_type_grouped = as.factor(analysis_dat$DOM_type_grouped)
DOM_type <- levels(analysis_dat$DOM_type_grouped)
DOM_type

n.studies= data.frame(
  DOM_type_grouped = DOM_type,
  n.stud = rep(NA,length(exp))
)

i=1
for(p in DOM_type){
  sub = analysis_dat$pub_ID[analysis_dat$DOM_type_grouped == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i+1
} 
n.studies

# 
# DOM_type_grouped         n.stud
# 1            fulvic acid      2
# 2             humic acid      4
# 3             lake water      3
# 4            metabolites      2
# 5 natural organic matter      1
# 6           stream water      3
# 7             wastewater      4

# number of data points:
analysis_dat %>% group_by(DOM_type_grouped) %>% summarize(n = n())

# DOM_type_grouped           n
# 1 fulvic acid               24
# 2 humic acid                43
# 3 lake water               128
# 4 metabolites               21
# 5 natural organic matter     6
# 6 stream water              34
# 7 wastewater                49


analysis_dat %>% group_by(DOM_conditioned, DOM_type_grouped, .add = TRUE) %>% summarize(n = n())
# DOM_conditioned DOM_type_grouped           n
# <fct>           <fct>                  <int>
#   1 no              fulvic acid               24
# 2 no              humic acid                43
# 3 no              lake water               127
# 4 no              natural organic matter     6
# 5 yes             lake water                 1
# 6 yes             metabolites               21
# 7 yes             stream water              34
# 8 yes             wastewater                49
 
#### END ####

