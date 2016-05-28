data_file <- "mad_diary_all_update19feb2015_merge_fionneke.csv"
moovd <- read.csv(data_file, header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
moovd


#indicator variable for the missing vars
#ind_appin<-as.numeric(is.na(moovd$increase_appetite))

#create new variable where the NA values are replaced by the second time slot values
#appin_new<-moovd$increase_appetite

result.appin <- list()

result.appin <- list()
result.appin <- list()
variable_name <- 'increase_appetite'
#variable_name <- 'mad_diary_1'
#moovd$increase_appetite[ind_appin == 0]
id_name <- 'subjno'
splitted_file <- split(moovd, moovd[[id_name]])
for (subject in splitted_file) {
  id <- subject[[id_name]][1]
  for (i in 1:dim(subject)[[1]]) {
    if(!all(is.na(subject[[variable_name]]))){
      result[[id]] <- subject[[i, variable_name]]
      result.appin <- list()
      result.appin <- list()
      
      break
    }  
  }
}

df <- data.frame(
  appin = result.appin,
  
)