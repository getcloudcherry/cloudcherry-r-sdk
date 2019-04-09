#' Export data from your CC dashboard
#'
#' This function allows you to export data by specifying what question Ids you want to export
#' @details Install the following packages- httr, jsonlite before you use this. Performance depends on how much data is present in the specified time frame.
#' @param Username your CC username
#' @param Password your CC password
#' @param RequiredQidsVect a vector of strings specifying what all qids you want to export
#' @param AfterDate String of start date in format - "YYYY-MM-DD"
#' @param BeforeDate String of end date in format - "YYYY-MM-DD"
#' @param MaxSampleSize specifies what is the maximum sample set you wish to export, by default returns all data
#' @param Randomize true if you wish to randomize the data in the given time range and then export. by default set to False
#' @param ReturnIntersectedSet set to true if you want to pull the data with an intersection (i.e., AND condition) between the Question ID's specified
#' @param Questionnaires this is a vector of questionnaires from which you would want to pull data
#' @keywords GetAnswers
#' @return R DataFrame with CC data
#' @export
#' @examples
#' GetAnswers("username", "password", c("59df48ea81180318b06d0199", "59df48ea81180318b06d019a"), "2018-01-14", "2018-05-15", 2000, TRUE, TRUE, c("Q1"))
GetAnswers <- function(Username, Password, RequiredQidsVect, AfterDate, BeforeDate, MaxSampleSize = 0, Randomize = FALSE, ReturnIntersectedSet = FALSE, Questionnaires = NULL)
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

  if(BearerToken$status_code != 200){
    print("Authentication Failed")
    return(NULL)
  }

  BearerTokenToChar = rawToChar(BearerToken$content)

  BearerTokenToJson = fromJSON(BearerTokenToChar)

  bearer = BearerTokenToJson[1]

  #-----------------------------

  url  <- "https://api.getcloudcherry.com"
  path <- "api/Questions/Active"

  Questions = GET(url, path = path, add_headers(Authorization = paste("Bearer", bearer, sep = " ")))

  if(is.null(Questions$content)){
    print("Unable to fetch questions- try again- if problem persists contact CloudCherry")
    return(NULL)
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
  }, error = function(e) {print(paste("Unable to identify- ", RequiredQidsVect[i], " check if QuestionID present in account", sep = "")); return(NULL)})

  url  <- "https://api.getcloudcherry.com"
  path <- "api/Answers"

  QuestionFilter = list()

  if(ReturnIntersectedSet == TRUE){
    for (i in 1:length(RequiredQidsVect))
    {
      if(NumberTypes[[i]] == TRUE){
        QuestionFilter[[i]] = list(questionId = RequiredQidsVect[i], answerCheck = list("gt"), number = -1)
      }
      else{
        QuestionFilter[[i]] = list(questionId = RequiredQidsVect[i], answerCheck = list("Any Text"))
      }
    }
  }

  LocationList = list()

  if (!is.null(Questionnaires) | length(Questionnaires) != 0){
    for (i in 1:length(Questionnaires)){
      LocationList[[i]] = Questionnaires[i]
    }
  }

  processDate = function(InputStringDate){

    InputStringDate = paste(InputStringDate, "23:59:59", sep = " ")

    d = as.POSIXct(InputStringDate, format="%Y-%m-%d %H:%M:%S")

    attr(d, "tzone") <- "UTC"

    return(gsub("[+]", ".", c(toString(strftime(d , "%Y-%m-%dT%H:%M:%S%zZ", tz = "UTC"))))[1])
  }

  AfterDate = processDate(AfterDate)

  BeforeDate = processDate(BeforeDate)

  body = list(location = LocationList, afterdate = AfterDate, beforedate = BeforeDate, filterquestions = QuestionFilter)

  Responses = POST(url, path = path, add_headers(Authorization = paste("Bearer", bearer, sep = " ")), body = body, encode = "json")

  ResponsesToChar = rawToChar(Responses$content)

  ResponsesToJson = fromJSON(ResponsesToChar)

  if (is.null(ResponsesToJson)){
    print("Unable to fetch responses. Make sure data is present in the account for the specified date range or try setting argument 'ReturnIntersectedSet' to False")
    return(NULL)
  }

  OutputDf = data.frame(matrix(vector(), 0, length(RequiredQidsVect) + 3,
                               dimnames=list(c(), c(c("ID", "DateTime", "Questionnaire"), RequiredQidsVect))),
                        stringsAsFactors=F)

  tryCatch(for (i in 1:nrow(ResponsesToJson))
  {
    row = list()

    row[[1]] = ResponsesToJson$id[i]
    row[[2]] = ResponsesToJson$responseDateTime[i]
    row[[3]] = ResponsesToJson$locationId[i]

    for (j in 1:length(RequiredQidsVect)){
      if (RequiredQidsVect[j] %in% ResponsesToJson[[7]][i][[1]]$questionId){
        if (NumberTypes[[j]] == TRUE){
          row[[j+3]] = ResponsesToJson[[7]][i][[1]][ResponsesToJson[[7]][i][[1]]$questionId == RequiredQidsVect[j],]$numberInput
        }
        else{
          row[[j+3]] = ResponsesToJson[[7]][i][[1]][ResponsesToJson[[7]][i][[1]]$questionId == RequiredQidsVect[j],]$textInput
        }
      }
      else{
        row[[j+3]] = NA
      }
    }

    OutputDf[i,] = row
  }, error = function(e) {print("Unable to fetch data"); return(NULL)})

  colnames(OutputDf) = c(c("ID", "DateTime", "Questionnaire"), RequiredQidsVect)

  if (Randomize == TRUE){

    if (MaxSampleSize == 0){
      MaxSampleSize = nrow(OutputDf)
    }

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
    if (MaxSampleSize == 0){
      return(OutputDf)
    }
    else{
      return(head(OutputDf, MaxSampleSize))
    }
  }
}
