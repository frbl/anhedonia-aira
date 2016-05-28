good_bad_count <- function(res, anhedonia, no_anhedonia) {
  if(!exists('res')){
    print("Var modellen nog niet berekend")
    return()
  }
  count.good <- 0
  count.bad <- 0
  count.anhedonia_good <- c()
  count.no_anhedonia_good <- c()
  count.other <- 0
  for (model in res) {
    print(paste('Processing',model$name))
    id = strsplit(model$name, "\\.")[[1]][1]
    if(class(model) == "av_state") {
      if(length(model$accepted_models) >= 1){
        count.good <- count.good + 1
        if(id %in% anhedonia)
          count.anhedonia_good <- c(count.anhedonia_good, id)
        else if(id %in% no_anhedonia)
          count.no_anhedonia_good <- c(count.no_anhedonia_good, id)
        else
          count.other <- count.other + 1
      }else {
        count.bad <- count.bad + 1
      }
    }else{
      if (model$bucket >= 0.01) {
        count.good <- count.good + 1
        if(id %in% anhedonia)
          count.anhedonia_good <- c(count.anhedonia_good, id)
        else if(id %in% no_anhedonia)
          count.no_anhedonia_good <- c(count.no_anhedonia_good, id)
        else
          count.other <- count.other + 1
      }else {
        count.bad <- count.bad + 1
      }
    }
  }
  
  print(paste("Percentage dat een valide model heeft: ", 100 * count.good / (count.good + count.bad), "%",sep =""))
  print(paste(100 * length(count.anhedonia_good) / (count.good + count.bad), "% zijn anhedonie personen",sep =""))
  print(count.anhedonia_good)
  
  print(paste(100 * length(count.no_anhedonia_good) / (count.good + count.bad), "% zijn niet-anhedonie personen",sep =""))
  print(count.no_anhedonia_good)
  if(count.other != 0) print("Blijkbaar zijn er ook mensen buiten de groepen? (dit zou niet mogelijk moeten zijn)")
  
  # This creates groups of equal size
  #min_length <- min(c(length(count.anhedonia_good), length(count.no_anhedonia_good)))
  #c(count.anhedonia_good[1:min_length], count.no_anhedonia_good[1:min_length])
  
  # Despite of whatever we calculated before this line, the following line contains the ids of those that are matched and valid
  cur_no_anhedonia <- c(102232, 104789, 110514, 110544, 107110, 109755, 112244, 111264, 111884, 105962, 110642, 104703, 111543, 109531, 109747, 110676, 109751, 105722, 104231, 106423)
  cur_anhedonia <- c(104517, 107596, 108172, 112028, 100849, 111939, 101912, 111745, 111779, 111464, 111473, 111566, 111737, 110360, 111184, 111492, 111459, 112052, 111268, 111350)

  print('We will be using this anhedoniagroup:')
  print(cur_anhedonia)
  print('We will be using this no-anhedoniagroup:')
  print(cur_no_anhedonia)
  
  c(cur_no_anhedonia, cur_anhedonia)
}
