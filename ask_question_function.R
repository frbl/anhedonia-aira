ask_question <- function(question) {
  ques <- readline(prompt=question)
  res <- FALSE
  if (exists('ques') & ques != "") {
    ques = tolower(ques)
    if(ques=='y'){
      res <- TRUE
    }
  }
  res
}
