options(shiny.sanitize.errors=FALSE)
library(httr)
library(jsonlite)
library(shinyjs)
library(promises)
library(future)
library("aws.s3")
library(shinyWidgets)
plan(multiprocess)
library(DT)
library(rlist)
library(stringr)
library(tools)
library(shinyBS)
library(shinydashboard)
readRenviron("./.Renviron")
source("cwfHelper.R")
options(onBookmarkState = list())
options(bookmarkValues = list())
options(parentStateId = list())
options(saveFileToPollyLock = NULL)
runningEnv <<- NULL
COMMIT_NOW <<- FALSE
options(LAST_COMMIT_MSG = list())
bookmarkSavingDir <<- "/srv/bookmarks/shiny/"
OS_SEP <<- "/"
SESSION_RESET <<- TRUE
TYPE_FILE_PATH = './.server'
IS_SERVER <<- FALSE
if (file.exists(TYPE_FILE_PATH)) {
  IS_SERVER <<- TRUE
}

# Installing some requirements that are not there automatically
if (!IS_SERVER) {
  if (!requireNamespace("shinytail", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github("colearendt/shinytail")
  }
  if (!requireNamespace("shinysky", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github("AnalytixWare/ShinySky")
  }
}

library(shinytail)
library(shinysky)

## Getting the logs in the correct form 
if (IS_SERVER) {
  logPathPolly <- paste0("find /var/log/shiny-server/", basename(getwd()), "*")
  logFilesForPolly <- system(logPathPolly, intern = TRUE)
  for (logFileForPollyi in 1:length(logFilesForPolly)) {
      cmdLinkFile <- paste0('ln -sf ', logFilesForPolly[logFileForPollyi], ' ', 'linkedLogs', toString(logFileForPollyi) , '.log')
      system(cmdLinkFile, intern = TRUE)
  }
  options(logFilesForPolly = system('find linkedLogs*.log', intern = TRUE))
}

# Setting options to upload the files to polly after user uplods
if (IS_SERVER) {
  shiny::shinyOptions(
    afterUpload = function(files) {
      isolate(upload_run_id <- getQueryString()$run_id)
      isolate(pollyshinyRunenv <- getQueryString()$env)
    }
  )
}

if (IS_SERVER) {
  shiny::shinyOptions(
    save.interface = function(id, callback) {
      dirname <- paste0(bookmarkSavingDir, getQueryString()$run_id, OS_SEP , id)
      if (utils::file_test('-d', dirname)) {
        stop("Directory ", dirname, " already exists")
      } else {
        dir.create(dirname, recursive = TRUE, mode = "0777")
        callback(dirname)
      }
    },
    load.interface = function(id, callback, run_id) {
      dirname <- paste0(bookmarkSavingDir, run_id, OS_SEP , id)
      if (!utils::file_test('-d', dirname)) {
        stop("Session ", id, " not found")
      } else {
        callback(dirname)
      }
    }
  )
}

#Over writing some shiny functions in the name space
if (IS_SERVER) {
  shinyInputLabel <- function(inputId, label = NULL) {
    tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      `for` = inputId
    )
  }

  toJSONShiny <- function(x, ...,  dataframe = "columns", null = "null", na = "null",
    auto_unbox = TRUE, digits = getOption("shiny.json.digits", 16),
    use_signif = TRUE, force = TRUE, POSIXt = "ISO8601", UTC = TRUE,
    rownames = FALSE, keep_vec_names = TRUE, strict_atomic = TRUE) {

    if (strict_atomic) {
      x <- I(x)
    }

    # I(x) is so that length-1 atomic vectors get put in [].
    jsonlite::toJSON(x, dataframe = dataframe, null = null, na = na,
    auto_unbox = auto_unbox, digits = digits, use_signif = use_signif,
    force = force, POSIXt = POSIXt, UTC = UTC, rownames = rownames,
    keep_vec_names = keep_vec_names, json_verbatim = TRUE, ...)
  }

  fileInput <- function(inputId, label, multiple = FALSE, accept = NULL,
    width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
    
    buttonStyle <- "color: #000;background-color: #FFF;box-shadow: none;"

    restoredValue <- restoreInput(id = inputId, default = NULL)

    # Catch potential edge case - ensure that it's either NULL or a data frame.
    if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
      warning("Restored value for ", inputId, " has incorrect format.")
      restoredValue <- NULL
    }

    if (!is.null(restoredValue)) {
      restoredValue <- toJSONShiny(restoredValue, strict_atomic = FALSE)
    }

    inputTag <- tags$input(
      id = inputId,
      name = inputId,
      type = "file",
      style = "display: none;",
      `data-restore` = restoredValue
    )

    if (multiple)
      inputTag$attribs$multiple <- "multiple"
    if (length(accept) > 0)
      inputTag$attribs$accept <- paste(accept, collapse=',')

    tags$div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      shinyInputLabel(inputId, label),

      tags$div(class = "input-group", style = "display: flex;",
        dropdownButton(
          tags$label(class = "input-group-btn polly_file_upload_button", style = "padding:5px;",
            tags$span(class = "btn btn-default btn-file", style = buttonStyle,
              "Import from computer",
              inputTag
            )
          ),
          tags$label(class = "input-group-btn polly_file_upload_button", style = "padding:5px;",
            tags$span(class = "btn btn-default btn-file polly_import_from_project", style = buttonStyle,
              "Import from Polly",
              polly_import_id = inputId,
              polly_import_type = inputTag$attribs$multiple
            )
          ),
          inputId = paste0(inputId,"_buttn"),
          label = buttonLabel,
          status = "primary",
          circle = FALSE
        ),
        tags$input(type = "text", class = "form-control",
          placeholder = placeholder, readonly = "readonly"
        )
      ),
      tags$div(
        id=paste(inputId, "_progress", sep=""),
        class="progress progress-striped active shiny-file-input-progress",
        tags$div(class="progress-bar")
      )
    )
  }
  assignInNamespace("fileInput", shiny::fileInput, ns = "shiny")
}


if (IS_SERVER) {
  downloadButton <- function(outputId,
                            label="Download",
                            class=NULL, ...) {

    dropdownButton(
      inputId = paste0(outputId,"_buttn"),
      label = label,
      status = "primary",
      circle = FALSE,
      tags$a(
        id=outputId,
        class=paste('shiny-download-link', class),
        href='',
        target='_blank',
        download=NA,
        icon("download"),
        "Download",
        ...
      ),
      tags$a(
        id=paste0("polly_push_to_projects", outputId),
        class=paste('shiny-download-link polly_add_to_project', class),
        href='javascript:void(0)',
        download=FALSE,
        icon("download"),
        "Save data to Polly",
        polly_download_id=outputId,
        ...
      )
    )
  }
  assignInNamespace("downloadButton", shiny::downloadButton, ns = "shiny")
}

# Javascript to R connector code
jsCode <- "
  shinyjs.getcookie = function(params) {
    var allCookies = {}; 
    for (var cookie  in Cookies.get()) {
      if (
        cookie.indexOf('CognitoIdentityServiceProvider') > -1 && 
        (cookie.indexOf('idToken') > -1 || cookie.indexOf('refreshToken') > -1)
      ) {
        allCookies[cookie.trim()] = Cookies.get(cookie).trim();
      }
    }
    allCookies = JSON.stringify(allCookies);
    Shiny.onInputChange('pollyCookies', allCookies);
  }
  shinyjs.bookmarkedAndSaved = function(params) {
    window.parent.postMessage('success','*')
  }
  shinyjs.reset = function() {
    history.go(0)
  }
  shinyjs.clearSelectedPollyProjectFiles = function() {
    Shiny.onInputChange('pollyProjectFileTable_rows_selected', []);
  }

  shinyjs.pollyUploadContainerVisiblity = function(params) {
    $('#' + params[0] + '_progress.shiny-file-input-progress').css('visibility', params[1] ? 'visible' : 'hidden');
  }

  shinyjs.pollyUploadProgress = function(params) {
    $('#' + params[0] + '_progress.shiny-file-input-progress .progress-bar').width(Math.round(params[1] * 100) + '%');
  }

  shinyjs.pollyUploadComplete = function(params) {
    Shiny.shinyapp.makeRequest('uploadEnd', [params[0], params[1]], function (response) {
    }, function (error) {
    });
  }

  shinyjs.setFileText = function(params) {
    $el =  $('#' + params[0])
    files = params[1]
    var $fileText = $el.closest('div.input-group').find('input[type=text]');
    if (files.length === 1) {
      $fileText.val(files[0].name);
    } else {
      $fileText.val(files.length + ' files');
    } 
  }
  polly_getfile_format_count = 0
  shinyjs.getAcceptedFileFormat = function(params) {
    polly_getfile_format_count = polly_getfile_format_count + 1;
    if (params.length > 0) {
      Shiny.setInputValue('polly_getfile_format_data', document.getElementById(params[0]).getAttribute('accept') + '#' + polly_getfile_format_count);
    } else {
      Shiny.setInputValue('polly_getfile_format_data', 'NO_FILE_FORMAT' + '#' + polly_getfile_format_count);
    }
  }

  shinyjs.pollyButtonClick = function(params) {
    document.getElementById(params[0]).click()
  }

  shinyjs.pollyMakeIdInvisible = function(params) {
    document.getElementById(params[0]).style.visibility = 'hidden';
  }

  shinyjs.pollyUploadDisable = function(params) {
    var button_id = '#' + params[0] + '_buttn_state button'
    $(button_id).attr('disabled', true)
  }
"

# Project table modifier
projectTableSorting <- "
  function(data, type, row, meta) {
    function bytesToSize(bytes) {
      var sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
      if (bytes == null) return '-';
      if (bytes == 0) return 'n/a';
      var i = parseInt(Math.floor(Math.log(bytes) / Math.log(1024)));
      if (i == 0) return bytes + ' ' + sizes[i];
      return (bytes / Math.pow(1024, i)).toFixed(1) + ' ' + sizes[i];
    };
    return bytesToSize(data)
  }
"

projectTableRemoveTimePrecision <- "
  function(data, type, row, meta) {
    if(type == 'display') {
      var dateRegex = new RegExp(/^[0-9]{4}[\\-]{1}[0-9]{2}[\\-]{1}[0-9]{2} [0-9]{2}[\\:][0-9]{2}[\\:][0-9]{2}[\\.][0-9]{6}$/g)
      if (dateRegex.test(data)) {
        return data.split('.')[0]
      }
    }
    return data
  }
"

getAllProjectFiles <- function(upload_env1, upload_run_id1, pollyCookies) {
  if (upload_env1 == 'prod') {
    apiUrl <- 'https://api.polly.elucidata.io'
  } else if (upload_env1 == 'test') {
    apiUrl <- 'https://api.testpolly.elucidata.io'
  } else if (upload_env1 == 'eupolly') {
    apiUrl <- 'https://api.eu-polly.elucidata.io'
  } else {
    apiUrl <- 'https://api.devpolly.elucidata.io'
  }

  requestUrl <- paste0(apiUrl, OS_SEP, 'run?state=project&id=', upload_run_id1)
  getRes <- fromJSON(httr::content(httr::GET( requestUrl, httr::set_cookies(unlist(fromJSON(pollyCookies)))), "text"))
  requestUrl <- paste0(apiUrl, '/project?id=', getRes$project$id)
  getRes <- fromJSON(httr::content(httr::GET( requestUrl, httr::set_cookies(unlist(fromJSON(pollyCookies)))), "text"))
  if (identical(nrow(getRes$project_files), NULL) || nrow(getRes$project_files) == 0) {
    return (getRes$project_files)
  }
  col_order <- c("file_name", "last_modified", "size")
  getRes$project_files <- getRes$project_files[, col_order]
  return (getRes$project_files)
}

getAllProjectFilesAndFolders <- function(upload_env1, upload_workspace_id1, pollyCookies, sub_path = "/") {
  if (upload_env1 == 'prod') {
    apiUrl <- 'https://apis.polly.elucidata.io/mithoo'
  } else if (upload_env1 == 'test') {
    apiUrl <- 'https://apis.testpolly.elucidata.io/mithoo'
  } else if (upload_env1 == 'eupolly') {
    apiUrl <- 'https://apis.eu-polly.elucidata.io'
  } else {
    apiUrl <- 'https://apis.devpolly.elucidata.io/mithoo'
  }

  apiKey <- Sys.getenv("POLLY_API_KEY")  # Get API key from environment variable

  requestUrl <- paste0(apiUrl, OS_SEP, 'workspaces', OS_SEP, upload_workspace_id1, OS_SEP, '_search')

  print(requestUrl)

  payload <- list(
    from = 0,
    size = 20,
    sort = list(
      list("entity_name.keyword" = list(order = "asc"))
    ),
    query = list(
      bool = list(
        should = list(
          list(
            bool = list(
              must = list(
                list(term = list(`_index` = list(value = "{document_index}"))),
                list(term = list(workspace_id = list(value = upload_workspace_id1))),
                list(term = list(parent_folder_name = list(value = paste0(upload_workspace_id1, URLencode(sub_path))))),
                list(bool = list(
                  should = list(
                    list(terms = list(entity_type = c("analysis", "notebook", "file", "folder")))
                  )
                ))
              )
            )
          ),
          list(
            bool = list(
              must = list(
                list(term = list(`_index` = list(value = "{workspace_index}"))),
                list(nested = list(
                  path = "permissions",
                  query = list(
                    match = list(`permissions.user_id` = "{user_id}")
                  )
                ))
              )
            )
          )
        )
      )
    )
  )


  getRes <- httr::content(httr::POST(
          requestUrl, 
          body = toJSON(payload, auto_unbox = TRUE), 
          encode = "json",
          httr::add_headers(`X-API-Key` = apiKey, `Content-Type` = "application/vnd.api+json")
      ))

  formatted_data <- list(
    data = lapply(getRes$hits$hits, function(item) {
      src <- item$`_source`
      list(
        type = src$entity_type,
        id = src$s3_key,
        attributes = list(
          s3_key = src$s3_key,
          last_modified = as.character(as.POSIXct(src$modified_date, origin = "1970-01-01", tz = "UTC")),
          size = ifelse(!is.null(src$size), paste0(round(src$size / 1024, 2), " KB"), "-"),
          file_name = src$entity_name
        ),
        links = src$links
      )
    })
  ) 

  if (!is.list(formatted_data$data)) {
        return (
        data.frame(file_name = character(),
        last_modified = character(),
        size = character(),
        file_type = character(),
        base_path = character(),
        stringsAsFactors=FALSE)
        )
    }

    # Convert JSON data into a structured data frame
    df <- do.call(rbind, lapply(formatted_data$data, function(row) {
    if (!is.list(row$attributes)) {
        return(NULL)  # Skip invalid entries
    }
    
    # Handle missing values properly
    file_type_name <- as.character(paste0(icon("file", lib = "glyphicon", class="project-files-size"), " ", row$attributes$file_name))
    if (identical(row$type, "folder")) {
      file_type_name <- as.character(paste0(icon("folder-close", lib = "glyphicon", class="project-files-size"), " ",row$attributes$file_name))
    }
    list(
        file_name = file_type_name,
        last_modified = ifelse(is.null(row$attributes$last_modified) || length(row$attributes$last_modified) == 0, "-", row$attributes$last_modified),
        size = ifelse(is.null(row$attributes$size), "-", row$attributes$size),
        file_type = ifelse(is.null(row$type), "-", row$type),
        file_name_correct = ifelse(is.null(row$attributes$file_name), "-", row$attributes$file_name),
        base_path = sub_path, stringsAsFactors=FALSE
    )
    }))

    # Convert to data frame
    df <- as.data.frame(df, stringsAsFactors = FALSE)

    # Print the result
    return(df)
}

prepareProjectData <- function(project_data_df, fileFormats) {
  result <- c()
  if(nrow(project_data_df) < 1) {
    return(project_data_df)
  }
  for (row in 1:nrow(project_data_df)) {
    fileName <- project_data_df[row, 'file_name']
    size_value <- as.character(project_data_df[row, 'size'])
    fileExt <- file_ext(fileName)
    if (grepl("TB", project_data_df[row, 'size'])) {
      project_data_df[row, 'size'] = as.numeric(strsplit(size_value, "TB")[1]) * 1000 * 1000 * 1000 * 1000
    } else if (grepl("GB", project_data_df[row, 'size'])) {
      project_data_df[row, 'size'] = as.numeric(strsplit(size_value, "GB")[1]) * 1000 * 1000 * 1000
    } else if (grepl("MB", project_data_df[row, 'size'])) {
      project_data_df[row, 'size'] = as.numeric(strsplit(size_value, "MB")[1]) * 1000 * 1000
    } else if (grepl("KB", project_data_df[row, 'size'])) {
      project_data_df[row, 'size'] = as.numeric(strsplit(size_value, "KB")[1]) * 1000
    } else if (grepl("B", project_data_df[row, 'size'])) {
      project_data_df[row, 'size'] = as.numeric(strsplit(size_value, "B")[1])
    }
  }
  project_data_df$size <- as.numeric(project_data_df$size)
  pollyTableData <- project_data_df
  
  if (length(result) > 0) {
    pollyTableData <- project_data_df[-result,]
    project_data_df <- pollyTableData
  }
  return(project_data_df)
}

showProjectDataTable <- function( pollyTableDataDf, input, fileFormat = "NO_FILE_FORMAT") {
  if (identical(fileFormat, "NO_FILE_FORMAT")) {
    rowCallback <- c(
      "function(row, data, displayNum, displayIndex){",
        "$(row).first().attr('title', data[0].split('</i>')[1].trim())",
        "$(row).find('td').first().addClass('polly-ellipses')",
        "if (data[0].indexOf('glyphicon-folder-close') > -1) {",
          "$(row).addClass('disabled-polly-folder');",
          "$(row).find('td').attr('style', 'background-color: #FFFFFF !important;');",
        "} else {",
          "$(row).addClass('disabled-element-click');",
        "}",
      "}"
    )
  } else {
    rowCallback <- c(
      "function(row, data, displayNum, displayIndex){",
        "function endsWithAny(suffixes, string) {",
          "return suffixes.some(function (suffix) {",
            "return string.endsWith(suffix);",
          "});",
        "}",
        "$(row).first().attr('title', data[0].split('</i>')[1].trim())",
        "$(row).find('td').first().addClass('polly-ellipses')",
        "if (data[0].indexOf('glyphicon-folder-close') > -1) {",
          "$(row).addClass('disabled-polly-folder');",
          "$(row).find('td').attr('style', 'background-color: #FFFFFF !important;');",
        "}",
        paste0("let formats = '",fileFormat,"'.split(',');"),
        "let requiredFormat = {};",
        "for (let fileFormatsasked of formats) {",
          "if(fileFormatsasked == 'text/csv' || fileFormatsasked == 'text/comma-separated-values') {",
            "requiredFormat['.csv'] = 1",
          "} else if (fileFormatsasked == 'text/plain') {",
            "requiredFormat['.txt'] = 1",
          "} else if (fileFormatsasked == 'application/vnd.ms-excel') {",
            "requiredFormat['.xls'] = 1",
          "} else if (fileFormatsasked == 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {",
            "requiredFormat['.xlxs'] = 1",
          "} else {",
            "requiredFormat[fileFormatsasked.toLowerCase()] = 1",
          "}",
        "}",
        paste0("if(!endsWithAny(Object.keys(requiredFormat), data[0].split('</i>')[1].trim().toLowerCase())) {"),
          "if (data[0].indexOf('glyphicon-folder-close') < 0) {",
            "$(row).addClass('disabled-element');",
          "}",
        "}",
      "}"
    )
  }
  DT::renderDataTable(DT::datatable(pollyTableDataDf[,c("file_name","last_modified","size")], class="project-data-table-class", rownames = FALSE, escape = FALSE,
      colnames = c('Name', 'Last modified', 'Size'), 
      options = list(
        rowCallback = JS(rowCallback),
        columnDefs = list(
          list( targets = -2, render = DT::JS(projectTableRemoveTimePrecision)), list( targets = -1, render = DT::JS(projectTableSorting)), list(width = '100px', targets = c(1)), list(width = '50px', targets = c(2))))),          
      selection = input$polly_import_type_from_project_data)
}

#Main function which will init the things that is needed to integare to polly
pollyEventInit <- function(session, input, output, variableList, reactivedata,
  callbacks = list()) {
   observeEvent(input$pollyCookies, {
    # higher priprity function which get called before users observe
    observe({
      # checking if the trail lisence part
      
      runningEnv <<- parseQueryString(session$clientData$url_search)$env
      if (IS_SERVER) {
        if (identical(runningEnv, 'prod')) {
          apiUrl <- 'https://apis.polly.elucidata.io/auth'
        } else if (runningEnv == 'test') {
          apiUrl <- 'https://apis.testpolly.elucidata.io/auth'
        } else if (runningEnv == 'eupolly') {
          apiUrl <- 'https://apis.eu-polly.elucidata.io/auth'
        } else {
          apiUrl <- 'https://apis.devpolly.elucidata.io/auth'
        }

        apiKey <- Sys.getenv("POLLY_API_KEY")  # Get API key from environment variable

        requestUrl <- paste0(apiUrl, '/me')
        getRes <- fromJSON(content(GET(requestUrl, add_headers(`X-API-Key` = apiKey)), "text"))

        trialEnabled <- getRes$organization_details$licenses[[1]]$is_trial
        trialDataSet <- NULL

        # if (identical(trialEnabled, TRUE)) {
        #   pollyRunId <- parseQueryString(session$clientData$url_search)$run_id
        #   requestUrl <- paste0(apiUrl, OS_SEP, 'run?id=', pollyRunId, '&state=detail')
        #   getRes <- fromJSON(content(GET(requestUrl, add_headers(`X-API-Key` = apiKey)), "text"))
          
        #   if (identical(grepl('demo#', getRes$run_type), TRUE)) {
        #     trialDataSet <- strsplit(getRes$run_type, "demo#")[[1]][[2]]
        #   }
        # }
        if (identical(trialEnabled, TRUE)) {
          pollyRunId <- parseQueryString(session$clientData$url_search)$run_id
          requestUrl <- paste0(apiUrl, OS_SEP, 'run?id=', pollyRunId, '&state=detail')
          getRes <- fromJSON(httr::content(httr::GET( requestUrl, httr::set_cookies(unlist(fromJSON(input$pollyCookies)))), "text"))
          if (identical(grepl('demo#', getRes$run_type), TRUE)) {
            trialDataSet <- strsplit(getRes$run_type, "demo#")[[1]][[2]]
          }
        }
        

        if (identical(trialEnabled, TRUE)) {
          if (!identical(trialDataSet, NULL)) {
            pollyExampleId <- getOption("pollyRunTrialRunExample")
            js$pollyButtonClick(pollyExampleId)
            js$pollyMakeIdInvisible(pollyExampleId)
            pollyInputFileListData <- getOption("pollyRunTrialDisable")
            for(btn_elm in pollyInputFileListData) {
              js$pollyUploadDisable(btn_elm)
            }
            pollyDisappearButtons <- getOption("pollyRunTrialDisappear")
            for(btn_rem in pollyDisappearButtons) {
              js$pollyMakeIdInvisible(btn_rem)
            }
          }
        }
      }
      if (length(getQueryString()$run_id) != 0) {
        tmp <- getOption("parentStateId")
        tmp[[getQueryString()$run_id]] <- parseQueryString(session$clientData$url_search)$parent_id
        options(parentStateId = tmp)
        if (!(getQueryString()$run_id %in% names(getOption("LAST_COMMIT_MSG")))) {
          tmp <- getOption("LAST_COMMIT_MSG")
          tmp[[getQueryString()$run_id]] <- ""
          options(LAST_COMMIT_MSG = tmp)
        }
      }

      if (IS_SERVER) {
        observeEvent(input$pollyCookies, {
          options(pollyCookies = input$pollyCookies)
        })

        # function related polly project files
        observeEvent(input$polly_add_to_project_data, {
          downloadFileBtn <- strsplit(input$polly_add_to_project_data,"#")[[1]][1]
          fileNameToBeVal <- session$downloads$get(downloadFileBtn)$filename
          fileNameTobe <- ""
          if (!identical(fileNameToBeVal, NULL)) {
            if (is.character(fileNameToBeVal) & length(fileNameToBeVal) == 1) {
              fileNameTobe <- fileNameToBeVal
            } else {
              fileNameTobe <- fileNameToBeVal()
            }
            extensFile <- paste0(".", file_ext(fileNameTobe))

            fileNameWithoutExt <- strsplit(fileNameTobe, extensFile)[[1]][1]
            
            polyReactiveData_internal$pollyProjectBasePath = "/"
            upload_env <- getQueryString()$env
            upload_run_id <- getQueryString()$`workspace-id`
            pollyCookies <- input$pollyCookies
            polyReactiveData_internal$pollyTableData <- getAllProjectFilesAndFolders(upload_env, upload_run_id, pollyCookies, polyReactiveData_internal$pollyProjectBasePath)
            polyReactiveData_internal$pollyTableData <- prepareProjectData(polyReactiveData_internal$pollyTableData)

            showModal(
              modalDialog(
                actionButton("pollyProjectRefresh", shiny::icon("sync"), class="polly-project-refresh"),
                uiOutput("projectFileUserInfo", style="font-size:16px;"),
                fluidRow(column(10, textInput("polly_save_to_project_input", NULL, value = fileNameWithoutExt, placeholder= "Enter the file name"), style="padding-right:0px; margin-top:20px;", class = "pollyShinyInputClass"), column(2,tags$b(extensFile), style="padding-left:0px;padding-top: 5px;font-size:16px; margin-top:20px;")),
                uiOutput("projectFileRoutesInfo", style="font-size:16px;"),
                uiOutput("projectFileRoutesUpload", style="font-size:16px;"),
                dataTableOutput("pollyProjectFileTable"),
                textOutput("pollyProjectImportError"),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("polly_save_to_projects_ok", "Save")
                )
              )
            )
            output$projectFileUserInfo <- renderUI({
              HTML("<strong>File will be named as: </strong>")
            })
            output$projectFileRoutesInfo <- renderUI({
              HTML("<strong>File will be uploaded to the path: </strong>")
            })
            output$projectFileRoutesUpload <- renderUI({
              a <- "/"
              for (logFileForPollyi in 1:length(strsplit(polyReactiveData_internal$pollyProjectBasePath, "/")[[1]])) {
                if (logFileForPollyi == "1" || logFileForPollyi == 1) {
                  split_data <- "home"
                  elm_id <- "go-to-polly-home"
                } else{
                  split_data <- strsplit(polyReactiveData_internal$pollyProjectBasePath, "/")[[1]][[logFileForPollyi]]
                  elm_id <- paste(head(strsplit(polyReactiveData_internal$pollyProjectBasePath, "/")[[1]], logFileForPollyi), sep = '/', collapse = '/')
                  elm_id <- paste0(elm_id, "/")
                }
                a <- paste0(a, "<a class='polly-project-route-click' style='cursor:pointer;' id='", URLencode(elm_id) ,"'>", split_data, "</a>", "/")
              }
              HTML(a)
            })
            output$pollyProjectFileTable <- showProjectDataTable(polyReactiveData_internal$pollyTableData, input, "NO_FILE_FORMAT")
            js$getAcceptedFileFormat()
          }
        })

        observeEvent(input$polly_save_to_projects_ok, {
          downloadFileBtn <- strsplit(input$polly_add_to_project_data,"#")[[1]][1]
          
          if ( identical(getOption("saveFileToPollyLock"), strsplit(input$polly_add_to_project_data,"#")[[1]][2])) {
            return ()
          }
          options(saveFileToPollyLock = strsplit(input$polly_add_to_project_data,"#")[[1]][2])

          fileNameToBeVal <- session$downloads$get(downloadFileBtn)$filename
          fileNameTobe <- ""
          if (is.character(fileNameToBeVal) & length(fileNameToBeVal) == 1) {
            fileNameTobe <- fileNameToBeVal
          } else {
            fileNameTobe <- fileNameToBeVal()
          }
          extensFile <- paste0(".", file_ext(fileNameTobe))
          fileNameToBeGiven <- paste0(input$polly_save_to_project_input, extensFile)
          if (fileNameToBeGiven %in% polyReactiveData_internal$pollyTableData$file_name_correct) {
            removeModal()
            showModal(
              modalDialog(
                fluidRow(column(10,tags$b("File name already exists would you like to replace the file?"))),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("polly_save_to_projects_replace", "Replace")
                )
              )
            )
            return()
          }

          downloadFileBtn <- strsplit(input$polly_add_to_project_data,"#")[[1]][1]
          subreq <- NULL
          subreq$PATH_INFO <- sprintf('/download/%s', URLencode(downloadFileBtn, TRUE))
          savingFileName <- fileNameToBeGiven
          fileToUploadPath <- session$handleRequest(subreq)[[3]]$file
          if(file.exists(fileToUploadPath)) {
            projetFileCompletePath <- paste0(polyReactiveData_internal$pollyProjectBasePath, "/", savingFileName)
            if (polyReactiveData_internal$pollyProjectBasePath == "/") {
              projetFileCompletePath <- paste0(polyReactiveData_internal$pollyProjectBasePath, savingFileName)
            }
            cmd <- paste0("polly files copy -s '", fileToUploadPath, "' -d ", "'polly:/", projetFileCompletePath, "' -y")
            if(system(cmd, intern = FALSE) == 0) {
              warning("File upload successfully")
            } else {
              warning("Not able to upload file")
            }
          } else {
            warning("Not able to find the file")    
          }
          removeModal()
          return()
        })

        observeEvent(input$polly_save_to_projects_replace, {

          downloadFileBtn <- strsplit(input$polly_add_to_project_data,"#")[[1]][1]
          fileNameToBeVal <- session$downloads$get(downloadFileBtn)$filename
          fileNameTobe <- ""
          if (is.character(fileNameToBeVal) & length(fileNameToBeVal) == 1) {
            fileNameTobe <- fileNameToBeVal
          } else {
            fileNameTobe <- fileNameToBeVal()
          }
          extensFile <- paste0(".", file_ext(fileNameTobe))
          fileNameToBeGiven <- paste0(input$polly_save_to_project_input, extensFile)
          downloadFileBtn <- strsplit(input$polly_add_to_project_data,"#")[[1]][1]
          subreq <- NULL
          subreq$PATH_INFO <- sprintf('/download/%s', URLencode(downloadFileBtn, TRUE))
          savingFileName <- fileNameToBeGiven
          fileToUploadPath <- session$handleRequest(subreq)[[3]]$file
          if (file.exists(fileToUploadPath)) {

            projetFileCompletePath <- paste0(polyReactiveData_internal$pollyProjectBasePath, "/", savingFileName)
            if (polyReactiveData_internal$pollyProjectBasePath == "/") {
              projetFileCompletePath <- paste0(polyReactiveData_internal$pollyProjectBasePath, savingFileName)
            }
            cmd <- paste0("polly files copy -s '", fileToUploadPath, "' -d ", "'polly:/", projetFileCompletePath, "' -y")
            if(system(cmd, intern = FALSE) == 0) {
              warning("File upload successfully")
            } else {
              warning("Not able to upload file")
            }
          } else {
            warning("Not able to find the file")    
          }
          removeModal()
          return()
        })
      }
      pollyInit(session, input, output, polyReactiveData_internal)
    }, priority = 100)
  })
  js$getcookie()

  # Usinf some restore calls backs from shiny
  onRestore(function(state) {
    onRestoreCopy(reactivedata, state, variableList)
    if (!is.null(callbacks$onrestore)) {
      callbacks$onrestore()
    }
  })

  onRestored(function(state) {
    if (!is.null(callbacks$onrestored)) {
      callbacks$onrestored()
    }
  })

  onBookmark(function(state) {
    if (!IS_SERVER) {
      # Code which tells what all variables has to e restored
      extractUserInput <- function(inputIdentifier) {
        inputIdentifierRegex <- paste0("sed -n '/", inputIdentifier, "/,/\\,/p' ui.R")
        userInputGrepData <- gsub("\"", "'", system(inputIdentifierRegex, intern = TRUE), fixed=TRUE)
        inputIdentifierRegex <- paste0("sed -n '/", inputIdentifier, "/,/\\,/p' server.R")
        userInputGrepDataServ <- gsub("\"", "'", system(inputIdentifierRegex, intern = TRUE), fixed=TRUE)
        if (!is.na(userInputGrepDataServ[1])) {
          userInputGrepData <- append(userInputGrepData, userInputGrepDataServ)
        }
        cleanedArray <- c()
        completeLine <- ""
        if (!is.na(userInputGrepData[1])) {
          for(i in 1:length(userInputGrepData)){
            if (grepl(inputIdentifier, userInputGrepData[i])) {
              if (!identical(completeLine, "")) {
                newStringIdentifier <- tail(strsplit(stringr::str_replace_all(strsplit(strsplit(gsub(" ", "", completeLine), inputIdentifier)[[1]][[2]], ",")[[1]][[1]], "\\(|'", ""), "=")[[1]], n = 1)
                if (grepl(newStringIdentifier, readChar("ui.R", file.info("ui.R")$size)) || grepl(newStringIdentifier, readChar("server.R", file.info("server.R")$size))) {
                  cleanedArray <- append(cleanedArray, c(newStringIdentifier))
                }
              }
              completeLine <- ""
              completeLine <- paste0(completeLine, userInputGrepData[i])
            } else {
              completeLine <- paste0(completeLine, userInputGrepData[i])
            }
          }
          if (!identical(completeLine, "")) {
            newStringIdentifier <- tail(strsplit(stringr::str_replace_all(strsplit(strsplit(gsub(" ", "", completeLine), inputIdentifier)[[1]][[2]], ",")[[1]][[1]], "\\(|'", ""), "=")[[1]], n = 1)
            if (grepl(newStringIdentifier, readChar("ui.R", file.info("ui.R")$size)) || grepl(newStringIdentifier, readChar("server.R", file.info("server.R")$size))) {
              cleanedArray <- append(cleanedArray, c(newStringIdentifier))
            }
          }
          return(cleanedArray)
        } else {
          return(NULL)
        }
      }

      # Code which tells what all variables has to e restored
      extractAllUserInput <- function() {
        allUserInputType <- c("checkboxGroupInput", "checkboxInput", "dateInput", "dateRangeInput", "numericInput", "selectInput", "sliderInput", "radioButtons", "textInput", "selectizeInput", "actionButton", "pickerInput")
        allVariables <- c()
        for (oneUserInputType in allUserInputType) {
          variableData <- extractUserInput(oneUserInputType)
          if (!identical(NULL, variableData)) {
            allVariables <- append(allVariables, variableData)
          }
        }
        allVariables <- unique(allVariables)
        return(allVariables)
      }
      dataToBeSaved <- extractAllUserInput()
      sortedListDataToBeSaved <- sort(variableList)
      sortedListDataUserGave <- sort(dataToBeSaved)
      if (!isTRUE(all.equal(sortedListDataToBeSaved, sortedListDataUserGave))) {
        extraDataSaved <- setdiff(sortedListDataToBeSaved, sortedListDataUserGave)
        dataNotSaved <- setdiff(sortedListDataUserGave, sortedListDataToBeSaved)
        stop(paste0("There is a problem in restore: Following variable should not be saved: ", paste(extraDataSaved, collapse=", "), " and you have missed restoring following variables: ", paste(dataNotSaved, collapse=", ")))
      }
    }

    onBookmarkCopy(session, reactivedata, state, input, variableList)
    if (!is.null(callbacks$onbookmark)) {
      callbacks$onbookmark()
    }
  })

  if (IS_SERVER && identical(environment(fun = session$ns)$ns_prefix, NULL)) {
    onBookmarked(function(state) {
      onBookmarkedCopy(session, input, state)
      if (!is.null(callbacks$onbookmarked)) {
        callbacks$onbookmarked()
      }
    })
  }
  polyReactiveData_internal <- reactiveValues(pollyTableData = NULL, pollyProjectDataResent = NULL, pollyProjectBasePath = "/")
  if (IS_SERVER && identical(environment(fun = session$ns)$ns_prefix, NULL)) {
    logFilesForPolly <- getOption('logFilesForPolly')
    lapply(1:length(logFilesForPolly), function(pollyLogFilei) {
      shinyPollyLogger <- reactiveVal(value = NULL)
      shinyPollyLoggerPr <- tailFile(logFilesForPolly[pollyLogFilei])
      observeEvent(input$pollyLogMainDropDown, {readStream(shinyPollyLogger, shinyPollyLoggerPr)})
      newNameLogg <- paste0('shinytail_pollyhelper', '_', toString(pollyLogFilei))
      output[[newNameLogg]] <- renderText({paste(shinyPollyLogger(), collapse = "\n")})
    })
  }
}


pollyInit <-  function(session, input, output, polyReactiveData_internal) {
  bookmarkModelInput(session, input)
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  if (IS_SERVER) {
    observeEvent(input$polly_import_from_project_data, {
      upload_env <- getQueryString()$env
      upload_workspace_id <- getQueryString()$`workspace-id`
      pollyCookies <- input$pollyCookies
      polyReactiveData_internal$pollyProjectBasePath <- "/"
      polyReactiveData_internal$pollyTableData <- getAllProjectFilesAndFolders(upload_env, upload_workspace_id, pollyCookies)
      
      if (!(identical(nrow(polyReactiveData_internal$pollyTableData), NULL) || nrow(polyReactiveData_internal$pollyTableData) == 0)) {
        importFileBtn <- strsplit(input$polly_import_from_project_data,"#")[[1]][1]
        js$getAcceptedFileFormat(importFileBtn)
      }
    })

    observeEvent(input$polly_getfile_format_data, {
      if (identical(strsplit(input$polly_getfile_format_data,"#")[[1]][1], "NO_FILE_FORMAT")) {
        return()
      }
      if (!(identical(nrow(polyReactiveData_internal$pollyTableData), NULL) || nrow(polyReactiveData_internal$pollyTableData) == 0)) {
        polyReactiveData_internal$pollyTableData <- prepareProjectData(polyReactiveData_internal$pollyTableData)
        
        showModal(
          modalDialog(
            actionButton("pollyProjectRefresh", shiny::icon("sync"), class="polly-project-refresh"),
            uiOutput("projectFileRoutes", style="font-size:16px;"),
            dataTableOutput("pollyProjectFileTable"),
            textOutput("pollyProjectImportError"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("polly_import_identifier_ok", "Import")
            )
          )
        )
        output$projectFileRoutes <- renderUI({
          a <- "/"
          for (logFileForPollyi in 1:length(strsplit(polyReactiveData_internal$pollyProjectBasePath, "/")[[1]])) {
            if (logFileForPollyi == "1" || logFileForPollyi == 1) {
              split_data <- "home"
              elm_id <- "go-to-polly-home"
            } else{
              split_data <- strsplit(polyReactiveData_internal$pollyProjectBasePath, "/")[[1]][[logFileForPollyi]]
              elm_id <- paste(head(strsplit(polyReactiveData_internal$pollyProjectBasePath, "/")[[1]], logFileForPollyi), sep = '/', collapse = '/')
              elm_id <- paste0(elm_id, "/")
            }
            a <- paste0(a, "<a class='polly-project-route-click' style='cursor:pointer;' id='", URLencode(elm_id) ,"'>", split_data, "</a>", "/")
          }
          HTML(a)
        })
        fileFomatOfFile <- strsplit(input$polly_getfile_format_data,"#")[[1]][1]
        output$pollyProjectFileTable <- showProjectDataTable(polyReactiveData_internal$pollyTableData, input, fileFomatOfFile)
      } else {
        showModal(
          modalDialog("No files are there in the Polly projects", footer = NULL)
        ) 
      }
    })

    observeEvent(input$project_folder_clicked, {
      if (identical(input$pollyProjectFileTable_rows_selected, NULL)) {
        return ()
      }
      folderRow <- polyReactiveData_internal$pollyTableData[tail(input$pollyProjectFileTable_rows_selected, n=1),]
      if (identical(folderRow$base_path, "/")) {
        polyReactiveData_internal$pollyProjectBasePath <- paste0("/" ,folderRow$file_name_correct)
      } else {
        polyReactiveData_internal$pollyProjectBasePath <- paste0(folderRow$base_path, "/" ,folderRow$file_name_correct)
      }
      upload_env <- getQueryString()$env
      upload_run_id <- getQueryString()$`workspace-id`
      pollyCookies <- input$pollyCookies
      pollyRefreshData <- getAllProjectFilesAndFolders(upload_env, upload_run_id, pollyCookies, polyReactiveData_internal$pollyProjectBasePath)
      polyReactiveData_internal$pollyTableData <- prepareProjectData(pollyRefreshData)
      fileFomatOfFile <- strsplit(input$polly_getfile_format_data,"#")[[1]][1]
      if (identical("NO_FILE_FORMAT", strsplit(input$polly_getfile_format_data,"#")[[1]][1])) {
        fileFomatOfFile <- "NO_FILE_FORMAT"
      }
      output$pollyProjectFileTable <- showProjectDataTable(polyReactiveData_internal$pollyTableData, input, fileFomatOfFile)
    })

    observeEvent(input$project_folder_route_clicked, {
      to_be_url <- URLdecode(strsplit(input$project_folder_route_clicked, "######")[[1]][1])
      if (to_be_url == "go-to-polly-home") {
        polyReactiveData_internal$pollyProjectBasePath <- "/"
      } else {
        polyReactiveData_internal$pollyProjectBasePath <- substr(to_be_url, 1, nchar(to_be_url) - 1)
      }
      upload_env <- getQueryString()$env
      upload_run_id <- getQueryString()$`workspace-id`
      pollyCookies <- input$pollyCookies
      pollyRefreshData <- getAllProjectFilesAndFolders(upload_env, upload_run_id, pollyCookies, polyReactiveData_internal$pollyProjectBasePath)
      polyReactiveData_internal$pollyTableData <- prepareProjectData(pollyRefreshData)
      fileFomatOfFile <- strsplit(input$polly_getfile_format_data,"#")[[1]][1]
      if (identical("NO_FILE_FORMAT", strsplit(input$polly_getfile_format_data,"#")[[1]][1])) {
        fileFomatOfFile <- "NO_FILE_FORMAT"
      }
      output$pollyProjectFileTable <- showProjectDataTable(polyReactiveData_internal$pollyTableData, input, fileFomatOfFile)
    })

    observeEvent(input$pollyProjectRefresh, {
      upload_env <- getQueryString()$env
      upload_run_id <- getQueryString()$`workspace-id`
      pollyCookies <- input$pollyCookies
      pollyRefreshData <- getAllProjectFilesAndFolders(upload_env, upload_run_id, pollyCookies, polyReactiveData_internal$pollyProjectBasePath)
      polyReactiveData_internal$pollyTableData <- prepareProjectData(pollyRefreshData)
      fileFomatOfFile <- strsplit(input$polly_getfile_format_data,"#")[[1]][1]
      if (identical("NO_FILE_FORMAT", strsplit(input$polly_getfile_format_data,"#")[[1]][1])) {
        fileFomatOfFile <- "NO_FILE_FORMAT"
      }
      output$pollyProjectFileTable <- showProjectDataTable(polyReactiveData_internal$pollyTableData, input, fileFomatOfFile)
    })

    observeEvent(input$polly_import_identifier_ok, {
      if (!(identical(nrow(polyReactiveData_internal$pollyTableData), NULL) || nrow(polyReactiveData_internal$pollyTableData) == 0)) {

        if (identical(input$pollyProjectFileTable_rows_selected, NULL)) {
          output$pollyProjectImportError <- renderText("Please select file(s) to import")
          return()
        }
        output$pollyProjectImportError <- renderText("")

        fileInfos <- polyReactiveData_internal$pollyTableData[input$pollyProjectFileTable_rows_selected,]
        preparedFileInfo = list()
        newfileInfos <- setNames(split(fileInfos, seq(nrow(fileInfos))), rownames(fileInfos))
        for (fileInfo in newfileInfos) {
          file_type <- NULL
          file_size <- 0
          if (grepl(".csv", fileInfo$file_name_correct)) {
            file_type <- "text/csv"
          } else if (grepl(".xls", fileInfo$file_name_correct)) {
            file_type <- "application/vnd.ms-excel"
          } else if (grepl(".xlxs", fileInfo$file_name_correct)) {
            file_type <- "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          }
          preparedFileInfo = list.append(preparedFileInfo, list(name = fileInfo$file_name_correct, size = ceiling(fileInfo$size), type = file_type))
        }

        subreq <- NULL
        subreq$PATH_INFO <- sprintf('/startimportpollyprojectsfiles/')
        subreq$REQUEST_METHOD <- "POST"
        subreq$FILE_INFO <- preparedFileInfo
        fileToUploadPath <- session$handleRequest(subreq)
        
        fileUploadCompleteCount <<- 0
        importFileBtn <- strsplit(input$polly_import_from_project_data,"#")[[1]][1]
        js$pollyUploadContainerVisiblity(importFileBtn, TRUE)
        js$setFileText(importFileBtn, preparedFileInfo)
        lapply(preparedFileInfo, function(fi) {
          subreq <- NULL
          subreq$PATH_INFO <- sprintf('/importpollyprojectsfile/')
          subreq$REQUEST_METHOD <- "POST"
          subreq$JOB_ID <- fileToUploadPath$content
          latestUploaded <- session$handleRequest(subreq)
          projetFileCompletePath <- paste0(polyReactiveData_internal$pollyProjectBasePath, "/", latestUploaded$content$name)
          if (polyReactiveData_internal$pollyProjectBasePath == "/") {
            projetFileCompletePath <- paste0(polyReactiveData_internal$pollyProjectBasePath, latestUploaded$content$name)
          }
          tempfilePathPolly <- dirname(latestUploaded$content$datapath)
          
          cmd <- paste0("mkdir -p ", tempfilePathPolly)
          
          if(system(cmd, intern = FALSE) == 0) {
            warning("Created folder")
          } else {
            warning("Folder creation error")
            return()
          }

          cmd <- paste0("polly files copy -s ", "'polly:/", projetFileCompletePath, "' -d '", latestUploaded$content$datapath, "' -y")
          if(system(cmd, intern = FALSE) == 0) {
            warning("File upload successfully")
          } else {
            warning("Not able to upload file")
            return()
          }
          subreq <- NULL
          subreq$PATH_INFO <- sprintf('/endimportpollyprojectsfile/')
          subreq$REQUEST_METHOD <- "POST"
          subreq$JOB_ID <- fileToUploadPath$content
          latestUploaded <- session$handleRequest(subreq)
          importFileBtn <- strsplit(input$polly_import_from_project_data,"#")[[1]][1]
          fileUploadCompleteCount <<- fileUploadCompleteCount + 1
          js$pollyUploadProgress(importFileBtn, fileUploadCompleteCount/length(preparedFileInfo))
        })
        js$pollyUploadComplete(fileToUploadPath$content, importFileBtn)
        removeModal()
      } else {
        removeModal()
      }
    })
  }
}

onBookmarkCopy <- function(session, reactiveDataList, onBookmarkIntState, appInput, variableArray) {
  tempBookMark <- list()
  if (IS_SERVER) {
    runIdPolly <- parseQueryString(session$clientData$url_search)$run_id
  } else {
    runIdPolly = '1'
  }
  if(!(runIdPolly %in% names(getOption("bookmarkValues")))) {
    tmp <- getOption("bookmarkValues")
    tmp[[runIdPolly]] <- NULL
    options(bookmarkValues = tmp)
  }

  for (arrayValue in variableArray) {
    dataToPut <- appInput[[arrayValue]]
    if ( length((appInput[[arrayValue]])) == 0 ) {
      dataToPut <- reactiveDataList[[arrayValue]]
    }

    if (identical(TRUE, dataToPut)) {
      dataToPut <- "TRUE"
    }
    
    if (identical(FALSE, dataToPut)) {
      dataToPut <- "FALSE"
    }

    if (identical(NULL, dataToPut)) {
      dataToPut <- "NULL"
    }

    if (identical(NA, dataToPut)) {
      dataToPut <- "NA"
    }

    onBookmarkIntState$values[[arrayValue]] <- dataToPut
    tempBookMark[[arrayValue]] <- dataToPut 
  }
  COMMIT_NOW <<- FALSE
  for (arrayValue in variableArray) {
    if (is.null(getOption("bookmarkValues")[[runIdPolly]])) {
      COMMIT_NOW <<- TRUE
      tmp <- getOption("bookmarkValues")
      tmp[[runIdPolly]] <- tempBookMark
      options(bookmarkValues = tmp)
      break
    } else if(is.null(getOption("bookmarkValues")[[runIdPolly]][[arrayValue]])) {
      COMMIT_NOW <<- TRUE
      tmp <- getOption("bookmarkValues")
      tmp[[runIdPolly]] <- tempBookMark
      options(bookmarkValues = tmp)
      break
    } else if (!identical(tempBookMark[[arrayValue]], getOption("bookmarkValues")[[runIdPolly]][[arrayValue]])) {
      COMMIT_NOW <<- TRUE
      tmp <- getOption("bookmarkValues")
      tmp[[runIdPolly]] <- tempBookMark
      options(bookmarkValues = tmp)
      break
    }
  }
}

onRestoreCopy <- function(reactiveDataList, onRestoreState, variableArray) {
  for (arrayValue in variableArray) {
    if (!identical(onRestoreState$values[[arrayValue]], NULL)) {
      reactiveDataList[[arrayValue]] <- onRestoreState$values[[arrayValue]]
    }
  }
}

onBookmarkedCopy <- function(session, input, onBookmarkedState) {
  showModal(bookmarkModel(session, onBookmarkedState))
}

storeVersionInPolly <- function(polly_run_id, pollyCookies, parent_state_id, onBookmarkedState, bookmarkedValues, enviro = 'test', verionName = 'latest_state') {
    apiUrl <- NULL
    if ( !length(polly_run_id) == 0 && !length(pollyCookies) == 0 &&  !length(parent_state_id) == 0) {
      if (enviro == 'prod') {
        apiUrl <- 'https://apis.polly.elucidata.io/mithoo-api'
      } else if (enviro == 'test') {
        apiUrl <- 'https://apis.testpolly.elucidata.io/mithoo-api'
      } else if (enviro == 'eupolly') {
        apiUrl <- 'https://apis.eu-polly.elucidata.io/mithoo-api'
      } else {
        apiUrl <- 'https://apis.devpolly.elucidata.io/mithoo-api'
      }

      options( scipen = 999 )

      postUrl <- paste0(apiUrl, '/uistores')
        postBody <- list(
            data = list(
                id = unbox("uistore"),
                type = unbox("uistore"),
                attributes = list(
                    runid = unbox(polly_run_id),
                    version_name = unbox(verionName),
                    parent_version = unbox(paste0(polly_run_id, '-', parent_state_id))
                )
            )
        )

      apiKey <- Sys.getenv("POLLY_API_KEY")  # Get API key from environment variable

      # requestUrl <- paste0(apiUrl, '/me')
      # getRes <- fromJSON(content(GET(requestUrl, add_headers(`X-API-Key` = apiKey)), "text"))
      
      postRes <- httr::content(httr::POST(
          postUrl, 
          body = toJSON(postBody, auto_unbox = TRUE), 
          encode = "json",
          httr::add_headers(`X-API-Key` = apiKey, `Content-Type` = "application/vnd.api+json")
      ))
      
      new_version_id <- postRes$data$id  # New response format

      # Construct PATCH request body
      patchReqBody <- list(
          storageId = unbox(strsplit(onBookmarkedState, "_state_id_=")[[1]][2]),
          storageMedium = unbox('shiny'),
          storageData = bookmarkedValues
      )

      patchUrl <- paste0(apiUrl, '/uistores/app-state/', new_version_id)
      patchRes <- httr::PATCH(
          patchUrl, 
          body = toJSON(list(payload = unbox(toString(toJSON(patchReqBody))))), 
          encode = "json",
          httr::add_headers(`X-API-Key` = apiKey, `Content-Type` = "application/vnd.api+json")
      )

      if (length(httr::content(patchRes)$version_id) != 0) {
        tmp <- getOption("parentStateId")
        tmp[[polly_run_id]] <- strsplit(httr::content(patchRes)$version_id, '-')[[1]][2]
        options(parentStateId = tmp)
      }
      options( scipen = 0 )
      options( digits = 6 )
    }
}

bookmarkModel <- function(session, state) {
  runIdPolly <- parseQueryString(session$clientData$url_search)$run_id
  tmp <- getOption("onBookmarkState")
  tmp[[runIdPolly]] <- state
  options(onBookmarkState = tmp)
  modalDialog(
    textInput("bookmark_identifier", "Description for the state",  value=""),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("bookmark_identifier_ok", "OK")
    )
  )
}

bookmarkModelInput <- function(session, input) {
  observeEvent(input$bookmark_identifier_ok, {
    runIdPolly <- parseQueryString(session$clientData$url_search)$run_id
    if (nzchar(input$bookmark_identifier)) {
      if (getOption("LAST_COMMIT_MSG")[[runIdPolly]] != input$bookmark_identifier) {
        COMMIT_NOW <<- TRUE
        if (COMMIT_NOW) {
          COMMIT_NOW <<- FALSE
          runIdPolly <- parseQueryString(session$clientData$url_search)$run_id
          storeVersionInPolly(runIdPolly, input$pollyCookies, getOption("parentStateId")[[runIdPolly]], getOption("onBookmarkState")[[runIdPolly]], getOption("bookmarkValues")[[runIdPolly]], runningEnv, input$bookmark_identifier)
          tmp <- getOption("LAST_COMMIT_MSG")
          tmp[[runIdPolly]] <- input$bookmark_identifier
          options(LAST_COMMIT_MSG = tmp)
          updateTextInput(session,"bookmark_identifier", value="")
          removeModal()
          js$bookmarkedAndSaved()
        } else {
          removeModal()
        }
      } else {
        removeModal()
      }
    }
  })
}

uiInit <- function() {
  list(
    tags$head(tags$script(src="https://elucidatainc.github.io/PublicAssets/builds/shiny/js.cookie.js")),
    tags$head(
        shiny::HTML(
          "
          <link rel='stylesheet' type='text/css' href='https://mithoo-public-data.s3-us-west-2.amazonaws.com/css/paper.min.css'>
          <link rel='stylesheet' type='text/css' href='https://mithoo-public-data.s3-us-west-2.amazonaws.com/css/shinydashboard.css'>
          <script src='https://mithoo-public-data.s3-us-west-2.amazonaws.com/css/shinydashboard.min.js'></script>
          <style>
          #keepAlive {
            visibility: hidden;
            height:0;
          }
          #shiny-modal {
            z-index: 100000;
          }
          header {
            display: none;
          }
          #pollyProjectImportError {
            color: crimson;
            font-size: large;
          }
          .pollyShinyInputClass > div {
            width: unset !important;
          }

          body, html {
            height: 100% !important;
          }
          .wrapper {
            height: 100% !important;          
          }
          .content-wrapper {
            min-height: 100% !important;
          }
          .left-side, .main-sidebar {
            padding-top: 0px !important;
          }
          .project-files-size {
            font-size: medium;
          }
          #polly_nav_toggle {
            position: absolute;
            right: 0;
            margin-right: -17px;
            top: 6px;
            background-color: #8477cf;
            height: 25px;
            color: #FFF;
            padding: 3px;
          }
          .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
            color: #fff;
            background: #1e282c;
            border-left-color: #8477cf;
          }
          .box.box-solid.box-primary>.box-header {
            color: #fff;
            background: #8477cf;
            background-color: #8477cf;
          }
          .collapsed-box > div > h3 {
            color: #FFF !important;
          }
          .box-header > h3 {
            color: #FFF !important;
          }
          .box.box-solid.box-primary {
            border: 1px solid #8477cf;
          }
          section.content {
            margin-left: 20px;
          }
          .morpheus-nav {
            display: none;
          }
          #pollyProjectFileTable > div > .dataTables_length{
            display: none !important;
          }
          #pollyProjectFileTable > div > .dataTables_filter {
            display: none !important;
          }
          #pollyProjectFileTable tr {
            cursor: pointer;
          }
          .polly-ellipses {
            white-space: nowrap; 
            overflow: hidden;
            max-width: 360px;
            width:360px;
            text-overflow: ellipsis;
          }
          .box.box-solid.box-warning > .box-header {
            background: #8477cf;
            border-color: #8477cf;
          }
          .box.box-solid.box-warning {
            border-color: #8477cf;
          }
          .btn-default:active, .btn-default.active, .open>.dropdown-toggle.btn-default {
            background-color: #8477cf !important;;
          }
          .btn-default:active:hover, .btn-default.active:hover, .open>.dropdown-toggle.btn-default:hover, .btn-default:active:focus, .btn-default.active:focus, .open>.dropdown-toggle.btn-default:focus, .btn-default:active.focus, .btn-default.active.focus, .open>.dropdown-toggle.btn-default.focus {
            background-color: #8477cf !important;
            color: #FFF;
          }
          .btn-default:active, .btn-default.active, .open>.dropdown-toggle.btn-default {
            color: #FFF;
          }
          .polly-project-refresh {
            float: right;
            margin: 10px;
            margin-left: 100% !important;
          }
          .box-header {
            background-color: #8477cf !important;
          }
          .box-solid {
            border: 1px solid #8477cf;            
          }
          #pollyLogMainDropDown {
            float: right;
          }
          #dropdown-menu-pollyLogMainDropDown {
            margin-top: 37px;
          }
          .btn-box-tool {
            color: #FFF !important;
          }
          .disabled-element-click {
            pointer-events: none;
          }
          .disabled-element {
            pointer-events: none;
            opacity: 0.4;
          }
          .disabled-polly-folder {
            background-color: #FFFFFF !important;
            cursor: pointer;
          }
          pre.shiny-text-output.noplaceholder.shiny-bound-output {
            color: #000 !important;
          }
          .btn {
            background-color:  #8477cf;
            color: #FFF;
          }
          .shinysky-busy-indicator {
            z-index: 1000000;
            width:100%;
            height:100%;
            top: 0 !important;
            left: 0!important;
            position: fixed;
            margin-top: auto;
            margin-left: auto;
            background: rgba(240, 240, 240, .4) !important;
            text-align: center;
            padding-top: 20px;
            padding-left: 30px;
            padding-bottom: 40px;
            padding-right: 30px;
            border-radius: 5px;
          }
          div.shinysky-busy-indicator > img {
            margin-left: auto;
            margin-right: auto;
            display: block;
            margin-top: auto;
          }
          div.shinysky-busy-indicator > p {
            font-size: xx-large;
            color: #000;
          }
          .panel-info>.panel-heading {
            color: #fff;
            background-color: #8477cf;;
            border-color: #8477cf;;
          }

          </style>
          "
        )
        ),
    tags$script(
      shiny::HTML(
        "
        var socket_timeout_interval
        var n = 0
        $(document).on('shiny:connected', function(event) {
        socket_timeout_interval = setInterval(function(){
        Shiny.onInputChange('count', n++)
        }, 30000)
        });
        $(document).on('shiny:disconnected', function(event) {
        clearInterval(socket_timeout_interval)
        });
        var polly_click_count = 0
        $(document).on('click', '.polly_add_to_project', function(event) {
          polly_click_count = polly_click_count + 1;
          Shiny.setInputValue('polly_add_to_project_data', event.target.getAttribute('polly_download_id') + '#' + polly_click_count);
          $(this).closest('.dropdown-menu').prev().dropdown('toggle');
          return false;
        });
        $(document).on('click', '.project-data-table-class > tbody > tr', function(event) {
          $(this).children('td').each((d) => {
            if($(this).children('td')[d].children.length > 0){
              isFolder = false;
              if($(this).children('td')[d].children[0].getAttribute('class').toString().indexOf('glyphicon-file') < 0) {
                isFolder = true;
                Shiny.setInputValue('project_folder_clicked', Math.round(Math.random() * 100000).toString());
              }
            }
          })
        });
        $(document).on('click', '.polly-project-route-click', function(event) {
          Shiny.setInputValue('project_folder_route_clicked', $(this).attr('id') + '######' + Math.round(Math.random() * 100000).toString())
        });
        $(document).on('click', '.polly_file_upload_button', function(event) {
          $('.dropdown-menu').prev().dropdown('toggle');
        });
        var polly_import_count = 0
        $(document).on('click', '.polly_import_from_project', function(event) {
          polly_import_count = polly_import_count + 1;
          Shiny.setInputValue('polly_import_type_from_project_data', event.target.getAttribute('polly_import_type') == 'multiple' ? 'multiple' : 'single' );
          Shiny.setInputValue('polly_import_from_project_data', event.target.getAttribute('polly_import_id') + '#' + polly_import_count);
          $(this).closest('.dropdown-menu').prev().dropdown('toggle');
          return false;
        });
        window.onload = function() {
          k =  `<a href='#' id='polly_nav_toggle'><span class='glyphicon glyphicon-chevron-left' style='top: -1px;'></span> <span class='glyphicon glyphicon-chevron-right' style='top: -1px;'></span></a>`
          if (document.getElementById('sidebarCollapsed')) {
            document.getElementById('sidebarCollapsed').appendChild($.parseHTML(k)[0]);
          }
          $(document).on('click', '#polly_nav_toggle', function() {
            $('.sidebar-toggle').click();
          })
        }
        "
      )
    ),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode,  functions = c("getcookie", "clearSelectedPollyProjectFiles", "bookmarkedAndSaved", "reset", "pollyUploadProgress", "pollyUploadContainerVisiblity", "pollyUploadComplete", "setFileText", "getAcceptedFileFormat", "pollyButtonClick", "pollyMakeIdInvisible", "pollyUploadDisable")),
    textOutput("keepAlive"),
    fluidRow(
      column(10,
        conditionalPanel(condition = "window.location.hostname != '127.0.0.1'",
          dropdownButton(
            tags$h5('Application logs'),
            lapply(1:length(getOption('logFilesForPolly')), function(i) {
              fluidRow(
                column(12,
                  shinyBS::bsCollapsePanel(paste0("Application log", ' ', toString(i)), 
                    shinyTail(paste0("shinytail_pollyhelper", '_', toString(i))), style = "info")
                )
              )
            }),
            circle = FALSE, label = 'Logs', width = '80%', right = TRUE, inputId = 'pollyLogMainDropDown'
          )
        ), offset = 0
      ),
      column(2,
        bookmarkButton("Save the state")
      )
    )
  )
  
}

pollyEnvsetUp <- function () {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

  options(repos=structure(BiocManager::repositories()))
  packrat::set_opts(local.repos = c("./privateDeps"))
  packrat::init()
}

pollyEnvSnapShot <- function(developerMode = TRUE) {
  makeFileChecksum <- function(pollyrDeveloperMode = TRUE) {
    if (pollyrDeveloperMode) {
      myCommand <- paste0("sha1sum config.yml pollyHelper.R cwfHelper.R > pollyrhelper_dev.txt")
      system(myCommand)
    } else {
      system("mv .circleci/config.yml config.yml")
      myCommand <- paste0("sha1sum config.yml pollyHelper.R cwfHelper.R > pollyrhelper_user.txt")
      system(myCommand)
      system("mv config.yml .circleci/config.yml")
      system("curl https://bitbucket.org/elucidatainc/pollyrhelper/raw/production/pollyrhelper_dev.txt > pollyrhelper_dev.txt")
      pollyHelperToUpdate <- system("cmp --silent   pollyrhelper_dev.txt pollyrhelper_user.txt || echo 'TRUE'", intern = TRUE)
      if (identical(pollyHelperToUpdate, "TRUE")) {
        print("There is an update in the polly integration. Updating...!")
        system("curl https://bitbucket.org/elucidatainc/pollyrhelper/raw/production/pollyHelper.R > pollyHelper.R")
        system("curl https://bitbucket.org/elucidatainc/pollyrhelper/raw/production/config.yml > .circleci/config.yml")
        system("curl https://bitbucket.org/elucidatainc/pollyrhelper/raw/production/cwfHelper.R > cwfHelper.R")
      }
      system("rm pollyrhelper_dev.txt pollyrhelper_user.txt")
    }
  }
  if (developerMode) {
    makeFileChecksum(FALSE)
    packrat::clean()
    packrat::snapshot(ignore.stale=TRUE)
  } else {
    makeFileChecksum()
  }
}
