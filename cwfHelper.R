getv2ApiUrl <- function(runningEnv) {
    upload_env <- runningEnv

    if (upload_env == 'prod') {
          apiUrl <- 'https://v2.api.polly.elucidata.io'
        } else if (upload_env == 'test') {
          apiUrl <- 'https://v2.api.testpolly.elucidata.io'
        } else if (upload_env == 'eupolly') {
          apiUrl <- 'https://v2.api.eu-polly.elucidata.io'
        } else {
          apiUrl <- 'https://v2.api.devpolly.elucidata.io'
        }
    return(apiUrl)
}

getbaseUrl <- function(runningEnv) {
    upload_env <- runningEnv

    if (upload_env == 'prod') {
          apiUrl <- 'https://polly.elucidata.io'
        } else if (upload_env == 'test') {
          apiUrl <- 'https://testpolly.elucidata.io'
        } else if (upload_env == 'eupolly') {
          apiUrl <- 'https://eu-polly.elucidata.io'
        } else {
          apiUrl <- 'https://devpolly.elucidata.io'
        }
    return(apiUrl)
}

getApiUrl <- function(runningEnv) {
    upload_env <- runningEnv

    if (upload_env == 'prod') {
          apiUrl <- 'https://api.polly.elucidata.io'
        } else if (upload_env == 'test') {
          apiUrl <- 'https://api.testpolly.elucidata.io'
        } else if (upload_env == 'eupolly') {
          apiUrl <- 'https://api.eu-polly.elucidata.io'
        } else {
          apiUrl <- 'https://api.devpolly.elucidata.io'
        }
    return(apiUrl)
}

getCWFStatus <- function(input, runningEnv) {

  OS_SEP <- '/'
  cwf_inst_unique_id = getQueryString()$workflow
  step_name = getQueryString()$step
  is_workflow <- FALSE
  is_first_step <- FALSE
  inputs <- list()
  apiUrl <- getv2ApiUrl(runningEnv)
  pollyCookies <- input$pollyCookies
  base_url <- getbaseUrl(runningEnv)
   if (!(is.null(cwf_inst_unique_id))){
    is_workflow = TRUE
    requestUrl <- paste0(apiUrl, OS_SEP, 'cwf_instances', OS_SEP, cwf_inst_unique_id)
    getRes <- fromJSON(httr::content(httr::GET( requestUrl, add_headers('Content-Type' = 'application/vnd.api+json'), httr::set_cookies(unlist(fromJSON(pollyCookies)))), "text"))
    steps <- getRes$data$attributes$step_details$steps
    inputs <- steps[[step_name]]$inputs
    
    if(length(steps) == 1){
    is_first_step <- TRUE
    }
  }
  values = list("base_url" = base_url,"is_workflow" = is_workflow, "cwf_inst_unique_id" = cwf_inst_unique_id,"is_first_step" = is_first_step,"inputs" = inputs)
  warning("102")
  warning(values)
  warning(pollyCookies)
  return(values)
}


savefileToPollyProjects = function(input ,fileName,fileToUploadPath){

  OS_SEP <<- "/"
  upload_run_id <- getQueryString()$run_id
  pollyCookies <- input$pollyCookies
  runningEnv <- getQueryString()$env
  apiUrl <- getApiUrl(runningEnv)
  fileName <- paste0(fileName,'_',upload_run_id)
  requestUrl <- paste0(apiUrl, OS_SEP, 'run?state=project&id=', upload_run_id)
  getRes <- fromJSON(httr::content(httr::GET( requestUrl, httr::set_cookies(unlist(fromJSON(pollyCookies)))), "text"))
  project_id <- getRes$project$id
  if(file.exists(fileToUploadPath)) {
    cmd <- paste0("polly files copy -s '", fileToUploadPath, "' -d ", "'polly://", fileName, "' -y")
    if(system(cmd, intern = FALSE) == 0) {
      warning("File upload successfully")
    } else {
      warning("Not able to upload file")
    }
  } else {
    warning("Not able to find the file")    
  }
  s3_file_path <- paste0(project_id,OS_SEP,fileName)
  return(s3_file_path)
  }

importfileFromPollyProjects <- function(input,fileToDownload, filepath){

  OS_SEP <<- "/"
  upload_run_id <- getQueryString()$run_id
  pollyCookies <- input$pollyCookies
  runningEnv <- getQueryString()$env
  apiUrl <- getApiUrl(runningEnv)
  project_id <- unlist(strsplit(as.character(fileToDownload), "\\/"))[1]
  fileName <- unlist(strsplit(as.character(fileToDownload), "\\/"))[2]
  requestUrl <- paste0(apiUrl, OS_SEP, 'project?state=get_upload_urls&id=', project_id)
  getRes <- fromJSON(httr::content(httr::GET( requestUrl, httr::set_cookies(unlist(fromJSON(pollyCookies)))), "text"))
  # Generate the signed file url for the file to be saved in the project
  signed_file_url <- str_replace(getRes$file_upload_urls, "[*]", fileName)
  
  cmd <- paste0('curl -X GET -o', filepath,' -H "x-amz-acl: bucket-owner-full-control" ', '"', signed_file_url, '"')
  if(system(cmd, intern = FALSE) == 0) {
    warning("File Downloaded successfully")
    } else {
        warning("Not able to download file")
    }
}

gotoNextStep <- function(input,outputs, runningEnv){
  
  apiUrl <- getv2ApiUrl(runningEnv)
  OS_SEP <- '/'
  cwf_inst_unique_id <- getQueryString()$workflow
  step_name <- getQueryString()$step
  step <- list("outputs" = outputs )
  steps <- list()
  steps[[step_name]]<- step
  step_details <- list('current_step' = unbox(step_name), 'steps' = steps)
  attributes <- list('step_details' = step_details)
  data <- list('type' = unbox('cwf_instances'), 'id' = unbox(cwf_inst_unique_id), 'attributes' = attributes)
  body <- list("data" = data)
  pollyCookies <- input$pollyCookies
  requestUrl <- paste0(apiUrl, OS_SEP, 'cwf_instances', OS_SEP, cwf_inst_unique_id)
  body_for_patch <- toJSON(body)
  getRes <- fromJSON(httr::content(httr::PATCH(requestUrl, body = body_for_patch, httr::set_cookies(unlist(fromJSON(pollyCookies))), add_headers("Content-Type" = "application/vnd.api+json")), "text"))
  warning("105")
  warning(getRes)
  return(getRes)
}