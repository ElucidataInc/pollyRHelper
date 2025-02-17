## Shiny Polly Helpeer
This has three files
1. pollyHelper.R -> File which integrates the app with polly
2. config.yml -> file which automates the deployment of shiny application to polly
3. pollyrhelper_dev.txt -> file which keeps track of the updates of the above two files do that devloper get there an update if exists

### App development Develpment
These files will not work alone this has to be added to a shiny application to get it working
1. Add the pollyHelper.R to the shiny application folder
2. source(pollyHelper.R) in the ui.R file
3. add the following files of code in the server. R file
```R
  pollyEventInit(session, input, output, variableList, v, list(onrestore = function() {
    flagvariables$pathway_name_flag <<- TRUE
    restoreFlag <<- TRUE
    flagvariables$protein_name_flag <<- TRUE
  }, onrestored = function() {
    restoreFlag <<- FALSE
    }
  ))
```
4. Add uiInit() to the ui function to the ui.R

Set to go

### After development
Before pushing the code run ```pollyEnvSnapShot(FALSE)``` this will make sure that other shiny application developer will get to know the update
while publishing to polly

### People to contact
1. Sabu George
2. Harshit Manek