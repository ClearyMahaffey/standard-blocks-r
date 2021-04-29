# setwd("Gatech/PUBP 3120/Final Project")
# Recommend you switch to your wd
#
# WARNING: For environmental hygiene, this code WILL clear your global
# environment. If this is a problem, please comment out 'remove(list=ls())' in
# the header.
#
# header ####################################################################
packNames = c("Rmisc","readxl", "tidyverse","openxlsx","broom")
for(i in 1:length(packNames)){
  if(!require(packNames[i], character.only = TRUE)){
    install.packages(packNames[i])
  }
  library(packNames[i], character.only = TRUE)
}
# package loader
remove(list=ls())
# clearing the environment
# Header end

# data loader ###############################################################

AllSheets <- {list(
  # importing data sources into a list
  
  read_excel("United Airport Data.xlsx",
             sheet = "airport_traffic", range = "A1:B21"),
  read_excel("United Airport Data.xlsx",
             sheet = "airport_revenue"),
  ## Airport data
  
  read_excel("United Airport Data.xlsx",
             sheet = "ga_unemployment"),  
  read_excel("United Airport Data.xlsx",
             sheet = "ga_gdp"),
  read_excel("United Airport Data.xlsx",
             sheet = "ga_respop"),
  read_excel("United Airport Data.xlsx",
             sheet = "ga_pcpi"),
  ## Georgia data
  
  read_excel("United Airport Data.xlsx",
             sheet = "fulton_unemployment"),
  read_excel("United Airport Data.xlsx",
             sheet = "fulton_gdp"),
  read_excel("United Airport Data.xlsx",
             sheet = "fulton_respop"),
  read_excel("United Airport Data.xlsx",
             sheet = "fulton_pcpi"),
  ## Fulton County data
  
  read_excel("United Airport Data.xlsx", 
             sheet = "atl_pop"),
  
  ## City of Atlanta data
  
  
  read_excel("United Airport Data.xlsx",
             sheet = "msa_unemployment"),
  read_excel("United Airport Data.xlsx",
             sheet = "msa_gdp"),
  read_excel("United Airport Data.xlsx",
             sheet = "msa_pop"),
  read_excel("United Airport Data.xlsx",
             sheet = "msa_pcpi")
  
  ## Atlanta metropolitican statistical area data
  
)}
# Importing data sources into a list
# <edit when committing example code: Yes, there are more elegant ways to do
# this. This was a quick-and-dirty fix to maximize legibility on an extremely
# tight schedule.>
## REMEMBER: make sure any 'year' columns are NOT capitalized

AirportData <- reduce(AllSheets, full_join, by = "year")
AirportData <- AirportData[order(AirportData$year,na.last = FALSE), ]
# merging all dfs into primary df and fixing any sorting problems

# data loader end

# regressions ###############################################################

ap_lm <- function(x){
  y <- lm(x ~ traffic + airport_revenue, na.action = na.omit, data = AirportData)
  return(y)
}
# building a function to make this cleaner

ga_unemployment_lm <- ap_lm(AirportData$ga_unemployment)
ga_gdp_lm <- ap_lm(AirportData$ga_gdp)
ga_pop_lm <- ap_lm(AirportData$ga_pop)
ga_pcpi_lm <- ap_lm(AirportData$ga_pcpi)
fulton_unemployment_lm <- ap_lm(AirportData$fulton_unemployment)
fulton_gdp_lm <- ap_lm(AirportData$fulton_gdp)
fulton_pop_lm <- ap_lm(AirportData$fulton_pop)
fulton_pcpi_lm <- ap_lm(AirportData$fulton_pcpi)
atl_pop_lm <- ap_lm(AirportData$atl_pop)
msa_unemployment_lm <- ap_lm(AirportData$msa_unemployment)
msa_gdp_lm <- ap_lm(AirportData$msa_gdp)
msa_pop_lm <- ap_lm(AirportData$msa_pop)
msa_pcpi_lm <- ap_lm(AirportData$msa_pcpi)

# running lm on all the variables.
# messy, but faster to write than automating it. May retune this with
# lapply later if I have time
# names are deliberately modular for later processing

lm_list <- list(ga_unemployment_lm,ga_gdp_lm,ga_pop_lm,ga_pcpi_lm,
                fulton_unemployment_lm,fulton_gdp_lm,fulton_pop_lm,fulton_pcpi_lm,
                msa_unemployment_lm,msa_gdp_lm,msa_pop_lm,msa_pcpi_lm,
                atl_pop_lm)

# building the list I would rather have auto-built up there

# regressions end

# regression text output ######################################################


lm_sheet_names <- c("ga_unemployment_lm","ga_gdp_lm","ga_pop_lm","ga_pcpi_lm",
               "fulton_unemployment_lm","fulton_gdp_lm","fulton_pop_lm","fulton_pcpi_lm",
               "msa_unemployment_lm","msa_gdp_lm","msa_pop_lm","msa_pcpi_lm",
               "atl_pop_lm")
# building a list of sheet names for later output into xlsx

tidy_lm_list <- lapply(lm_list,tidy, conf.int = TRUE, conf.level = 0.95)
write.xlsx(tidy_lm_list,"Airport Regression Data.xlsx",
           col.names = TRUE, row.names = FALSE, colWidths = c(NA,"auto","auto"),
           sheetName = lm_sheet_names)
# outputting main regression data

glance_lm_list <- lapply(lm_list, glance)
write.xlsx(glance_lm_list,"Airport Regression Extras.xlsx",
          col.names = TRUE, row.names = FALSE, colWidths = c(NA,"auto","auto"),
          sheetName = lm_sheet_names)
# outputting other important data (R-squared, sigma, degrees of freedom, etc)

# plots ######################################################################


airport_econ_indicators <- list("traffic","airport_revenue")
regions <- list("ga","fulton","msa")
econ_indicators <- list("unemployment","gdp","pop","pcpi")

# var list for the three-component files (airportEconInd_regions_econInd.png)

airport_plot <- function(x_var,y_var,plot_title,x_lab,y_lab){
p <- ggplot(AirportData,
            aes(
              x = eval(as.name(x_var)),
              y = eval(as.name(y_var))
              )
              # Using eval(as.name()) to pass strings from our lists into the 
              # function and evaluate them as variables.
            # Didn't have time to worry too much about colors and aesthetics -
            # would do something prettier in a larger project without competing
            # deadlines from other classes.
            ) + 
  xlab(x_lab) +
  ylab(y_lab) +
  # labels
  geom_point() + 
  geom_smooth(method='lm') +
  # actual points and lines
  ggtitle(plot_title)
  # plot title - thankfully this doesn't suffer from the multiplot title issue I
  # had to deal with later.
return(p)
}
# custom function to make our standardize plots. Using eval(as.name()) to pass
# strings from our lists into the function and evaluate them as variables.

for(i in airport_econ_indicators){
  for(j in econ_indicators){
    png(filename = paste(paste(i,j,sep = "_"),".png",sep=""),
        # double paste so things end in ".png" rather than "_.png"
        width = 1920,
        height = 1080,
        units = "px",
        pointsize = 12,
        bg = "white")
    print({
      multiplot(plotlist = 
                  c(lapply(regions,function(x){
                    airport_plot(i,
                                 paste(x,j,sep="_"),
                                 paste(i,"vs",x,j,sep = " "),
                                 i,
                                 x)
                  }
                  )),
                layout = matrix(c(0,1,2,3), nrow = 2, byrow=TRUE),
                cols = 2
                # Using titles caused strange behavior (extra blank plots).
                # Therefore, adding them manually in the report.
      )
    })
    dev.off()
  }
}
# nested for loops in R because sometimes I like to watch the world burn
# somewhere, future me is screaming about lapply
# <edit for example code commit: I am in fact.>
#
# 3-part file name:
#
# lapply: Using the list of econ_indicators, make a list of ggplots using
#   custom function airport_plot, one for each indicator (unemployment, gdp,
#   population, per capita personal income)
#
# Then, using that list from lapply, create a multiplot for that region, and
#   output it to a png named for the airport economic indicator and the region
#   (for instance, traffic_ga.png)
#
# Repeat this process for each region (GA, Fulton county, Atlanta MSA) from 
#   'regions'
#
# Repeat all for each airport economic indicator (traffic, revenue) from 
#   'airport_econ_indicators'
#
# strongly recommend use of the expand/collapse feature in R Studio for 
# readability