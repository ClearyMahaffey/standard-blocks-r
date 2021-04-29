packNames <- c("broom", "tidyverse", "readxl", "janitor")
for(i in 1:length(packNames)){
  if(!require(packNames[i], character.only = TRUE)){
    install.packages(packNames[i])
  }
  library(packNames[i], character.only = TRUE)
}