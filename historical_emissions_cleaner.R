packNames <- c("broom", "tidyverse", "readxl", "janitor")
for(i in 1:length(packNames)){
  if(!require(packNames[i], character.only = TRUE)){
    install.packages(packNames[i])
  }
  library(packNames[i], character.only = TRUE)
}

clean_fun <- function(x){
  emit <- read_csv(paste(x,".csv", sep = ""))
  emit2 <- data.frame(t(emit[-1]))
  emit3 <- emit[1:(length(emit2) - 2), c(1,3:length(emit))]
  # Cleaning junk data - removing last two rows (sources) and second column (units)
  emit3 <- data.frame(t(emit3))
  # transposing
  emit3 <- row_to_names(emit3, row_number = 1)
  # Setting row 1 to colnames
  emit3 <- rownames_to_column(emit3, "Year")
  # moving year to a true row, then giving it a "Year" colname
  write.csv(emit3, paste(x,"cleaned.csv", sep = "_"))
}
# writing cleaner function for these data

clean_fun("ghg-emissions-total")
clean_fun("ghg-emissions-sector")
clean_fun("ghg-emissions-sector&subsector")
clean_fun("ghg-emissions-energy")