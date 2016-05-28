selected_columns <- c("mad_diary_1",
                      "mad_diary_2",
                      "mad_diary_3",
                      "mad_diary_4",
                      "mad_diary_5",
                      "mad_diary_6",
                      "mad_diary_7",
                      "mad_diary_8",
                      "mad_diary_9",
                      "mad_diary_10",
                      "mad_diary_11",
                      "mad_diary_12",
                      "mad_diary_13",
                      "mad_diary_14",
                      "mad_diary_15",
                      "mad_diary_16",
                      "mad_diary_17",
                      "mad_diary_18",
                      "mad_diary_19",
                      "mad_diary_20",
                      "mad_diary_21",
                      "mad_diary_22",
                      "mad_diary_23",
                      "mad_diary_24",
                      "mad_diary_25",
                      "mad_diary_26",
                      "mad_diary_27",
                      "mad_diary_28",
                      "mad_diary_29",
                      "mad_diary_30",
                      "mad_diary_31",
                      "mad_diary_32",
                      "mad_diary_33",
                      "mad_diary_34",
                      "mad_diary_35",
                      "mad_diary_36",
                      "mad_diary_37",
                      "mad_diary_38",
                      "mad_diary_39",
                      "mad_diary_40",
                      "mad_diary_41",
                      "mad_diary_42",
                      "mad_diary_43")

names <- list( 'hoe_gaat_het_met_u' = 1,
               'heeft_u_sinds_het_vorige_meetmoment_geslapen' = 2,
               'slaapkwaliteit' = 3,
               'heeft_u_lang_genoeg_geslapen' = 4,
               'ik_voel_me_ontspannen' = 5,
               'ik_voel_me_somber' = 6,
               'ik_voel_me_energiek' = 7,
               'ik_voel_me_angstig' = 8,
               'ik_voel_me_enthousiast' = 9,
               'ik_voel_me_onrustig' = 10,
               'ik_voel_me_tevreden' = 11,
               'ik_voel_me_prikkelbaar' = 12,
               'ik_voel_me_kalm' = 13,
               'ik_voel_me_lusteloos' = 14,
               'ik_voel_me_opgewekt' = 15,
               'ik_voel_me_moe' = 16,
               'lichamelijk_ongemak' = 17,
               'ik_voel_me_gewaardeerd' = 18,
               'eenzaamheid' = 19,
               'tekortschieten' = 20,
               'ik_voel_me_zelfverzekerd' = 21,
               'piekeren' = 22,
               'ik_ben_snel_afgeleid' = 23,
               'levenslust' = 24,
               'ik_ben_van_slag' = 25,
               'ik_leef_in_het_hier_en_nu' = 26,
               'eetlust' = 27,
               'drukte' = 28,
               'waar_was_ik' = 29,
               'wat_deed_ik' = 30,
               'plezierigheid_activiteit' = 31,
               'is_er_iets_bijzonders_gebeurd' = 32,
               'waar_had_dit_mee_te_maken' = 33,
               'ik_was_het_afgelopen_dagdeel_grotendeels' = 34,
               'ik_was_liever_in_gezelschap_geweest' = 35,
               'ik_zou_liever_alleen_zijn_geweest' = 36,
               'ik_vond_dit_gezelschap_overwegend' = 37,
               'humor' = 38,
               'iets_betekenen' = 39,
               'buiten_zijn' = 40,
               'beweging' = 41,
               'ik_deed_dingen_op_de_automatische_piloot_zonder_me_bewust_te_zijn_van_wat_ik_deed' = 42)



convert_data <- function(data) {
  avgs <<- apply(data, 1, simple_avg)
  do.call(rbind, lapply(avgs, data.frame, stringsAsFactors=FALSE))
}

simple_avg <- function(measurement) {
  my_list <- list()
  
  #my_list$meetmoment   <- measurement['time']
  # my_list$missing      <- missing
  #my_list$dagdeel      <- convert_number_to_day_part(measurement)
  #my_list$datum        <- date.to_s
  #my_list$weekdag      <- convert_number_to_week_day(measurement)
  #my_list$somberheid           <- rounded_average(measurement, c(6, 14, 16))
  #my_list$onrust               <- rounded_average(measurement, c(8, 10, 12))
  #my_list$ontspanning          <- rounded_average(measurement, c(5, 11, 13))
  #my_list$opgewektheid         <- rounded_average(measurement, c(7, 9, 15))
  #my_list$eigenwaarde          <- rounded_average(measurement, c(18, 21))
  #my_list$concentratie         <- rounded_average(measurement, c(-23))
  #my_list$hier_en_nu           <- rounded_average(measurement, c(26, -42))
  #my_list$beweging             <- as.numeric(get_value(measurement, names$beweging))
  #my_list$iets_betekenen       <- as.numeric(get_value(measurement, names$iets_betekenen))
  #my_list$humor                <- as.numeric(get_value(measurement, names$humor))
  #my_list$buiten_zijn          <- as.numeric(get_value(measurement, names$buiten_zijn))
  #my_list$levenslust           <- as.numeric(get_value(measurement, names$levenslust))
  #my_list$lichamelijk_ongemak  <- as.numeric(get_value(measurement, names$lichamelijk_ongemak))
  #my_list$tekortschieten       <- as.numeric(get_value(measurement, names$tekortschieten))
  #my_list$piekeren             <- as.numeric(get_value(measurement, names$piekeren))
  #my_list$eenzaamheid          <- as.numeric(get_value(measurement, names$eenzaamheid))
  
  # Fionneke variablen
  my_list$date                 <- convert_date(measurement['mad_diary_invited_at'])
  my_list$pa                   <- rounded_average(measurement, c(5, 7, 9, 11, 13, 15))
  my_list$na                   <- rounded_average(measurement, c(6, 8, 10, 12, 14, 16))
  my_list$pa_activation        <- rounded_average(measurement, c(7, 9, 15))
  my_list$pa_deactivation      <- rounded_average(measurement, c(5, 11, 13))

  my_list$na_activation        <- rounded_average(measurement, c(8, 10, 12))
  my_list$na_deactivation      <- rounded_average(measurement, c(6, 14, 16))
  my_list$not_na_activation    <- 100 - rounded_average(measurement, c(8, 10, 12))
  my_list$not_na_deactivation  <- 100 - rounded_average(measurement, c(6, 14, 16))

  my_list$activity             <- as.numeric(get_value(measurement, names$beweging))
  my_list$upset                <- as.numeric(get_value(measurement, names$ik_ben_van_slag))
  my_list$not_upset            <- 100 - as.numeric(get_value(measurement, names$ik_ben_van_slag))
  my_list
  
}

convert_date <- function(dat) {
  dat <- strsplit(dat, '/')
  d <- dat[[1]][2]
  m <- dat[[1]][1]
  y <- dat[[1]][3]
  paste(y,m,d, sep='-')
}

get_value <- function(measurement, question_number) {
  measurement[value_prefix(question_number)]
}

value_prefix <-function(question_number) {
  paste("mad_diary", abs(question_number), sep="_")
}

inverse_value <- function(number) {
  if(!is.na(number)) 100 - number
  else NA
}

convert_number_to_week_day <- function(measurement) {
  day_number = measurement['week_day']
  switch(day_number,
         '1'='maandag',
         '2'='dinsdag',
         '3'='woensdag',
         '4'='donderdag',
         '5'='vrijdag',
         '6'='zaterdag',
         '7'='zondag') ############## klopt dit??
}

convert_number_to_day_part <- function(measurement) {
  day_part_number = measurement['week_day']
  res = ""
  if(measurement['morning'] == 1){
    res = "ochtend"
  } else if(measurement['afternoon'] == 1) {
    res = "middag"
  } else if(measurement['evening'] == 1) {
    res = "avond"
  }
  res
}

rounded_average <- function(measurement, question_numbers) {
  questions <- sapply(question_numbers, value_prefix)
  current_data <- measurement[questions]
  current_data <- sapply(current_data, as.numeric)
  
  for(i in 1:length(question_numbers)) {
    if(!is.na(current_data[i]) && question_numbers[i] < 0  ) {
      current_data[i] <- inverse_value(current_data[i])
    }
  }
  
  mean(current_data, na.rm = TRUE)
}

categorize_duration <- function(value) {
  res = NA
  
  if(is.na(value))
    res = NA
  else if (findInterval(value, c(0,33.333) ) == 1)
    res = 'Te kort'
  else if (findInterval(value, c(33.333, 66.666) ) == 1)
    res = 'Genoeg'
  else
    res = 'Te lang'
  res
}

valid_id <- function(id, ids_to_keep) {
  id %in% ids_to_keep
}

create_csvs <- function(data_file, ids_to_keep) {
  minimal_measurement_count = 1 || round(0.75*90)
  
  data.raw <- read.csv(data_file, header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
  
  data.sanitized <- data.raw
  
  # Only use period 1 data
  data.sanitized <- data.sanitized[data.sanitized$period == 1,]
  
  # Only use data filled out within the hour, the single pipe is used to do elementwise OR
  data.sanitized[(is.na(data.sanitized$time_completed) | data.sanitized$time_completed > 60), selected_columns] <- NA
  
  # Only use measurements having > minimal_measurement_count measurements
  data.sanitized <- data.sanitized[data.sanitized$observations > minimal_measurement_count,]
  
  # Remove trailing measurements if a person has > 90
  data.sanitized <- data.sanitized[data.sanitized$time <= 90,]
  
  # Extra measure, check if there are still 999's in the data, by looking at mad_diary_1
  test_variable <- na.omit(data.sanitized[(data.sanitized$mad_diary_1 >= 999), "mad_diary_1"])
  if(length(as.vector(test_variable))) stop('The data still contains 999 values!!!')
  
  # Split the data per patient
  data.splitted <- split(data.sanitized, data.sanitized$patient_id)
  unlink('csv', recursive = TRUE, force = TRUE)
  dir.create('csv')
  setwd('csv')
  
  for(i in 1:length(data.splitted)) {
    current <- data.splitted[[i]]
    id <- current$id[1]
    if(!is.na(id) && nrow(data.splitted[[i]]) >= minimal_measurement_count && valid_id(id, ids_to_keep)) {
      converted_data = convert_data(current)
      # NaNs in the dataframe will be handled gracefully and are converted to NAs.
      write.csv(converted_data, file = paste(id, "csv", sep="."))
    }
  }
  setwd('../')
}
