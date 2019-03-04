#' Export Questions present from your CC dashboard
#' 
#' This function allows you to export all questions or from a select questionnaire
#' @details Install the following packages- httr, jsonlite before you use this. Performance depends on how much data is present in the specified time frame.
#' @param Username your CC username
#' @param Password your CC password
#' @param Questionnaire Questionnaire from which you want to retrieve questions. will retrieve all by default
#' @keywords GetQuestions
#' @return R DataFrame with CC data
#' @export
#' @examples
#' GetQuestions("username", "password", "Test Questionnaire")
GetQuestions <- function(Username, Password, Questionnaire = NULL){
  if("httr" %in% rownames(installed.packages()) == FALSE) {install.packages("httr")}
  if("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages("jsonlite")}
  
  library(httr)
  library(jsonlite)
  
  options(stringsAsFactors = FALSE)
  
  #Login token---------------------
  
  Loginbody = list('username' = Username,'password' = Password, 'grant_type'='password')
  
  BearerToken <- POST('https://api.getcloudcherry.com/api/LoginToken',
                      body=Loginbody, encode = 'form')
  
  if(BearerToken$status_code != 200){
    return(print("Authentication Failed"))
  }
  
  BearerTokenToChar = rawToChar(BearerToken$content)
  
  BearerTokenToJson = fromJSON(BearerTokenToChar)
  
  bearer = BearerTokenToJson[1]
  
  #-----------------------------
  
  url  <- "https://api.getcloudcherry.com"
  path <- "api/Questions/Active"
  
  Questions = GET(url, path = path, add_headers(Authorization = paste("Bearer", bearer, sep = " ")))
  
  if(is.null(Questions$content)){
    return(print("Unable to fetch questions- try again- if problem persists contact CloudCherry"))
  }
  
  QuesToChar = rawToChar(Questions$content)
  
  QuesToJson = fromJSON(QuesToChar)
  
  OutputDf = data.frame(matrix(vector(), 0, 4,
                               dimnames=list(c(), c("QuestionID", "QuestionText", "DisplayNote", "Questionnaire"))),
                        stringsAsFactors=F)
  
  tryCatch(for (i in 1:nrow(QuesToJson))
  {
    row = list()
    
    if (is.null(Questionnaire) == T){
      OutputDf = rbind(OutputDf, data.frame(QuestionID = QuesToJson$id[i], QuestionText = QuesToJson$text[i], DisplayNote = QuesToJson$note[i], Questionnaire = paste(QuesToJson$displayLocation[i][[1]], collapse = ", ")))
      next
    }
    else if (length(QuesToJson$displayLocation[i][[1]]) == 0){
      OutputDf = rbind(OutputDf, data.frame(QuestionID = QuesToJson$id[i], QuestionText = QuesToJson$text[i], DisplayNote = QuesToJson$note[i], Questionnaire = NA))
      next
    }
    else if (Questionnaire %in% as.vector(QuesToJson$displayLocation[i][[1]])){
      OutputDf = rbind(OutputDf, data.frame(QuestionID = QuesToJson$id[i], QuestionText = QuesToJson$text[i], DisplayNote = QuesToJson$note[i], Questionnaire = paste(QuesToJson$displayLocation[i][[1]], collapse = ", ")))
      next
    }
  }, error = function(e) {print("Unable to fetch questions- try again- if issue persists contact CloudCherry"); return(NULL)})
  
  return(OutputDf)
}