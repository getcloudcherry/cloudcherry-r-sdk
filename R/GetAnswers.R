#' Export data from your CC dashboard
#' 
#' This function allows you to export data by specifying what question Ids you want to export
#' @details Install the following packages- httr, jsonlite before you use this. Performance depends on how much data is present in the specified time frame.
#' @param Username your CC username
#' @param Password your CC password
#' @param RequiredQidsVect a vector of strings specifying what all qids you want to export
#' @param AfterDate String of start date in format - "YYYY-MM-DD"
#' @param BeforeDate String of end date in format - "YYYY-MM-DD"
#' @param MaxSampleSize specifies what is the maximum sample set you wish to export
#' @param Randomize true if you wish to randomize the data in the given time range and then export
#' @keywords GetCCDataSet
#' @return R DataFrame with CC data
#' @export
#' @examples
#' GetCCDataSet("username", "password", c("59df48ea81180318b06d0199", "59df48ea81180318b06d019a"), "2018-01-14", "2018-05-15", 2000, TRUE)
GetAnswers <- function(Username, Password, RequiredQidsVect, AfterDate, BeforeDate, MaxSampleSize, Randomize)
{
  
  if("httr" %in% rownames(installed.packages()) == FALSE) {install.packages("httr")}
  if("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages("jsonlite")}
  
  library(httr)
  library(jsonlite)
  
  options(stringsAsFactors = FALSE)
  
  #Login token---------------------
  
  Loginbody = list('username' = Username,'password' = Password, 'grant_type'='password')
  
  BearerToken <- POST('https://api.getcloudcherry.com/api/LoginToken',
                      body=Loginbody, encode = 'form')
  
  BearerTokenToChar = rawToChar(BearerToken$content)
  
  BearerTokenToJson = fromJSON(BearerTokenToChar)
  
  bearer = BearerTokenToJson[1]
  
  #-----------------------------
  
  url  <- "https://api.getcloudcherry.com"
  path <- "api/Questions/Active"
  
  Questions = GET(url, path = path, add_headers(Authorization = paste("Bearer", bearer, sep = " ")))
  
  if (Questions$status_code != 200){
    return(print("Authentication Failed"))
  }
  
  if(is.null(Questions$content)){
    return(print("Unable to fetch questions"))
  }
  
  QuesToChar = rawToChar(Questions$content)
  
  QuesToJson = fromJSON(QuesToChar)
  
  QidTypeDf = cbind(do.call(what = "rbind",
                            args = lapply(QuesToJson[[1]], as.data.frame)), do.call(what = "rbind",
                                                                                    args = lapply(QuesToJson[[8]], as.data.frame)))
  
  colnames(QidTypeDf) = c("id", "type")
  
  NumberTypes = list()
  
  tryCatch(for (i in 1:length(RequiredQidsVect)){
    if (QidTypeDf[QidTypeDf$id == RequiredQidsVect[i],]$type %in% c("Smile-5", "Star-5", "Number", "Scale", "Slider")){
      NumberTypes[[i]] = TRUE
    }
    else{
      NumberTypes[[i]] = FALSE
    }
  }, error = function(e) {print("Unable to identify qids specified"); return(NULL)})
  
  url  <- "https://api.getcloudcherry.com"
  path <- "api/Answers"
  
  QuestionFilter = list()
  
  for (i in 1:length(RequiredQidsVect))
  {
    if(NumberTypes[[i]] == TRUE){
      QuestionFilter[[i]] = list(questionId = RequiredQidsVect[i], answerCheck = list("gt"), number = -1)
    }
    else{
      QuestionFilter[[i]] = list(questionId = RequiredQidsVect[i], answerCheck = list("Any Text"))
    }
  }
  
  body = list(afterdate = AfterDate, beforedate = BeforeDate, filterquestions = QuestionFilter)
  
  Responses = POST(url, path = path, add_headers(Authorization = paste("Bearer", bearer, sep = " ")), body = body, encode = "json")
  
  ResponsesToChar = rawToChar(Responses$content)
  
  ResponsesToJson = fromJSON(ResponsesToChar)
  
  if (is.null(ResponsesToJson)){
    return(NULL)
  }
  
  df = do.call(what = "rbind", args = lapply(ResponsesToJson[[7]], as.data.frame))
  
  datetime_df = do.call(what = "rbind", args = lapply(ResponsesToJson[[4]], as.data.frame))
  colnames(datetime_df) = c("DateTime")
  
  respid_df = do.call(what = "rbind", args = lapply(ResponsesToJson[[1]], as.data.frame))
  colnames(respid_df) = c("ID")
  
  DataList = list()
  
  tryCatch(for (i in 1:length(RequiredQidsVect))
  {
    if(NumberTypes[[i]] == TRUE){
      DataList[[i]] = as.vector(df[df$questionId == RequiredQidsVect[i],]$numberInput)
    }
    else{
      DataList[[i]] = as.vector(df[df$questionId == RequiredQidsVect[i],]$textInput)
    }
  }, error = function(e) {print("Unable to fetch data"); return(NULL)})

  OutputDf = as.data.frame(DataList)
  colnames(OutputDf) = RequiredQidsVect
  
  OutputDf = cbind(respid_df, datetime_df, OutputDf)
  
  if (Randomize == TRUE){
    
    if (MaxSampleSize > nrow(OutputDf)){
      SampledData = OutputDf[sample(nrow(OutputDf), nrow(OutputDf), replace = F), ]
    }
    else{
      SampledData = OutputDf[sample(nrow(OutputDf), MaxSampleSize, replace = F), ]
    }
    
    rownames(SampledData) = NULL
    
    return(SampledData)
  }
  else{
    return(head(OutputDf, MaxSampleSize))
  }
}
