#' @title Prepare packages and general functions
#' 
#' Packages installing and loading and the functions
#' general to the application

#' Package installing and loading.
#' @export
#' 
packageLoading <- function() {
  if(!"shiny" %in% rownames(installed.packages())){
    install.packages("shiny")
  }
  library(shiny)
  if(!"shinydashboard" %in% rownames(installed.packages())){
    install.packages("shinydashboard")
  }
  library(shinydashboard)
  if(!"httr" %in% rownames(installed.packages())){
    install.packages("httr")
  }
  library(httr)
  if(!"jsonlite" %in% rownames(installed.packages())){
    install.packages("jsonlite")
  }
  library(jsonlite)
  if(!"ggplot2" %in% rownames(installed.packages())){
    install.packages("ggplot2")
  }
  library(ggplot2)
  if(!"dplyr" %in% rownames(installed.packages())){
    install.packages("dplyr")
  }
  library(dplyr)
  if(!"reshape2" %in% rownames(installed.packages())){
    install.packages("reshape2")
  }
  library(reshape2)
  ## if(!"shinyBS" %in% rownames(installed.packages())){
  ##   install.packages("shinyBS")
  ## }
  ## library(shinyBS)

## install.packages("animation")
## library(animation)
## devtools::install_github("dgrtwo/gganimate")
## library("devtools")
## install.packages("devtools")
## library("devtools")
## devtools::install_github("dgrtwo/gganimate")
## library("gapminder")
}

#' Check directory structure for the data
#' @export
createDirectories <- function() {
  if(!file.exists('jiradata')) dir.create(file.path('jiradata'))
  if(!file.exists('jiradata/worklogsIndividual')) dir.create(file.path('jiradata/worklogsIndividual'))
  if(!file.exists('jiradata/issuesIndividual/')) dir.create(file.path('jiradata/issuesIndividual/'))
}

#' Tests if the machine has a valid IP.
#' @export
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ip addr show", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}

#' Tests if the Jira ip address is reachable by ping.
#' @export
#' 
canPingJira <- function() {
  if (.Platform$OS.type == "windows") {
!as.logical(system(paste0("ping -n 1 -w 1 ", jiraIp)))
  } else {
    !as.logical(system(paste0("ping -c1 -s1 -W1 -q ", jiraIp)))
  }
}

#' Assigns the same story points to a subtask which does not have.
#' @export
storyPoints <- function(key, i) {
  theRow <- grep(key, i$issues$key)
  storyPoints <- NULL
  storyPoints <- i$issues$fields$customfield_10008[theRow]
  if(is.na(storyPoints)) {
    theParentKey <- i$issues$fields$parent$key[theRow]
    if (!is.na(theParentKey)) {
      theParentRow <- grep(theParentKey, i$issues$key)
      storyPoints <- i$issues$fields$customfield_10008[theParentRow]
    } else {
      storyPoints <- 0
    }
  }
  storyPoints
}

#' Assigns the same story points to all subtask which does not have.
#' @export
setStoryPoints <- function(issues) {
  r <- NULL
  message("length of issues: ", length(issues))
  for ( i in 1:length(issues) ) {
    r <- c(r, storyPoints(issues$issues$key[i], issues))
  }
  r
}

#' Gets the start and end date of the sprint.
#' @export
getSprintData <- function() {
  if(canPingJira()){
    sprint <- GET(paste0("http://",jiraIp,":",jiraPort,"/rest/greenhopper/1.0/sprint/",sprintId,"/edit/model")
                  , authenticate(Username, Password, "basic")
                  , add_headers("Content-Type" = "application/json"))
    sprint <- fromJSON(content(sprint, "text"))
    save(sprint, file="jiradata/sprint")
  } else {
    load("jiradata/sprint")
  }
  sprint
  ## curl -D- -uricardo.olvera:xxxxxx -XGET -H "Content-Type: application/json" "http://10.220.108.3:8080/rest/greenhopper/1.0/rapidview"
  ## curl -D- -uricardo.olvera:xxxxxx -XGET -H "Content-Type: application/json" "http://10.220.108.3:8080/rest/greenhopper/1.0/sprint/46/edit/model"
}

loadData <- function() {
  u <<- getUsers()
  sprint <<- getSprintData()
    sprintStart <<- as.Date(
        sprint$sprint$startDate,
        format="%d/%b/%y %H:%M %p")
    sprintEnd <<- as.Date(
        sprint$sprint$endDate,
        format="%d/%b/%y %H:%M %p")
    dates <<- as.Date(
        sprint$sprint$startDate,
        format="%d/%b/%y %H:%M %p") + 0:21
    weekend <<- weekdays(dates) %in% c("Saturday", "Sunday")
    i1 <<- getIssuesJson()
    awl <<- getAllWorklogs(i1$issues$key)
    issuesDF <<- buildDFissues(i1)
    usersDF <<- getUserDF(u)
    usersDF <<- removeEmptyUsers(issuesDF, usersDF)
    issuesJson <<- getIndividualIssuesJson(issuesDF$'Issue key')
    cat(file=stderr(), "Total amount of issues: ", nrow(issuesDF), "\n")
}

list_field_user = list(
    tabPanel("test2",
             fluidRow(column(6,numericInput("inputtest", "test", value = 0),
                             column(6,actionButton(inputId ="test1",label ="go")))))
    ,h1("1234")
    ,h2("234")
)

list_field_admin = list( h1("admin"),h2("admin"))
