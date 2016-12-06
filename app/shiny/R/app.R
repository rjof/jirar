#' @title  app.R.
#' Some to does
#'

## * worklog cadence
## The /worklog/ individual logs come only with the full issue
## - Select user
## - Download evey issue
## - Construct the graph of the daily log time with all issues
## * Check directories creation (issuesIndividual & worklogsIndividual)
## * Decide if it might be a package or not
## * Go on with the documentation
## * Personal plots "Burn" and "Time spent" reactive to issue status

#' Packages importation
#'
#' @import shiny
#' @import shinydashboard
#' @import httr
#' @import jsonlite
#' @import ggplot2
#' @import dplyr
#' @import reshape2
## library(shiny)
## library(shinydashboard)
## library(httr)
## library(jsonlite)
## library(ggplot2)
## library(dplyr)
## library(reshape2)

## The next two lines are necesary for roxygen2::roxygenise()
## to run... Why?!!!
#' @name testingname
NULL

rm(list=ls())
##setwd("R/") ## Required by devtools::document() but not by roxygen2::roxygenise()
source("general.R")
packageLoading()
createDirectories()
source("users.R")
source("worklog.R")
source("issues.R")
source("parameters.R")
options(digits.sec=3)
## Needed to port it in a R portable with spanish al LC
## Sys.setlocale("LC_TIME", "English")
##loadData()

USER <- NULL
USER$Logged <- FALSE

ui <- dashboardPage(
    dashboardHeader(title = "Jira sprint analyzeer")
   ,dashboardSidebar(
        sidebarMenu(
            menuItem("Team", tabName = "team", icon = icon("dashboard"))
           ,menuItem("Personal", tabName = "widgets", icon = icon("th"))
           ,menuItem("Plan", tabName = "calendar", icon = icon("calendar"))
           ,uiOutput('teamMembers')
            ##           ,verbatimTextOutput("selectTeamMemberValue")
           ,checkboxGroupInput('checkboxgroupIssueStatus2'
                              ,'Status:'
                              ,c('To Do', 'In Progress', 'Done')
                              ,inline = TRUE
                              ,selected = c('To Do', 'In Progress', 'Done'))
        )
    )
   ,dashboardBody(
        uiOutput("page"),
        tabItems(
            ## First tab content
            tabItem(tabName = "team",
                    fluidRow(
                        box(plotOutput("timespent", height = 350))
                       ,box(plotOutput("teamBurn", height = 350))
                    )
                   ,fluidRow(
                        box(plotOutput("estimationError", height = 350))
                       ,box(plotOutput("storyPointsVSestimationError", height = 350))
                    )
                    ## ,fluidRow(
                    ##      box(plotOutput("plot1", height = 250))
                    ##     ,box(
                    ##          title = "Controls"
                    ##         ,sliderInput("slider", "Number of observations:", 1, 100, 50)
                    ##      )
                    ##  )
                    ),
            ## Second tab content
            tabItem(tabName = "widgets",
                    h2("Personal graphs")
                   ,fluidRow(
                        box(plotOutput("worklog", height=350))
                       ,box(plotOutput("personalBurn", height=350))
                       ,box(plotOutput("originalVSspent", height=350))
                    )
                   ,tableOutput('table')
                    )
           ,
            ## Third tab content
            tabItem(tabName = "calendar",
                    h2("Personal analysis to plan next sprint")
                   ,fluidRow(
                        h3("Story point")
                       ,p("Is a arbitrary measure used by Scrum teams.")
                       ,p("This is used to measure the effort required to implement a story. In simple terms its a number that tells the team how hard the story is. Hard could be related to complexity, unknowns and effort")
                       ,p("https://agilefaq.wordpress.com/2007/11/13/what-is-a-story-point/")
                        ## ,box(plotOutput("", height=350))
                        ## ,box(plotOutput("", height=350))
                        ## ,box(plotOutput("", height=350))
                    )
                    ## ,tableOutput('table')
                    )
        )
    )
)

server <- function(input, output) {
    USER <- reactiveValues(Logged = FALSE,role=NULL)

    ui1 <- function(){
        tagList(
            div(id = "login",
                wellPanel(textInput("userName", "Your jira username"),
                          passwordInput("passwd", "Your jira password"),
                          br(),actionButton("Login", "Log in")))
            ##,tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
        )}
    
    ui2 <- function(){list(tabPanel("Test",get_ui(USER$role)[2:3]),get_ui(USER$role)[[1]])}

    observe({ 
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <<- isolate(input$userName)
                    Password <<- isolate(input$passwd)
                    if (!is.null(Username) && !is.null(Password)) {
                        USER$Logged <- TRUE
                    }
                }
            }
        }
    })

    observe({
        if (USER$Logged == FALSE) {
            output$page <- renderUI({
                box( div(class="outer",do.call(bootstrapPage,c("",ui1())))) })
        }
        if (USER$Logged == TRUE) {
            loadData()
            output$teamMembers = renderUI({
                selectInput("selectTeamMember"
                           ,label = h3("Team members")
                           ,choices = getUserChoices(usersDF)
                           ,selected = Username
                            )
            })

            output$timespent <- renderPlot({
                e <- environment()
                iPlot <- issuesDF[!is.na(issuesDF[,7]),]
                iPlot <- group_by(iPlot, `Assignee name`)
                iPlot2 <- summarize(iPlot,sum(`Spent (h)`))
                iPlot <- iPlot2[order(iPlot2$"sum(`Spent (h)`)"),]
                g <- NULL
                g <- ggplot(iPlot, aes(x=reorder(iPlot$"Assignee name", iPlot$"sum(`Spent (h)`)"), y=iPlot$"sum(`Spent (h)`)", fill = iPlot$'Assignee name'), environment = e)
                g <- g + geom_bar(stat="identity")
                g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1))
                g <- g + guides(fill = FALSE)
                g <- g + ylab("Time spent (h)")
                g <- g + xlab("")
                print(g)
            })

            output$storyPointsVSestimationError <- renderPlot({
                e <- environment()
                iDFsp <- issuesDF[issuesDF[,"Story points"] > 0, ]
                iDFsp<-iDFsp[order(iDFsp$`Aggregated estimate (h)`),]
                t1<-as.data.frame(table(iDFsp$`Story points`))
                names(t1) <- c("Story p.","# issues")
                t2 <- aggregate(cbind(`Aggregated estimate (h)`, `Aggregated estimate error (h)`) ~ `Story points`, iDFsp, sum)
                t2<-cbind(t2,Count=t1[,2])
                t2<-mutate(t2, `AEE Pounded`=`Aggregated estimate error (h)`/`Count`)
                xmin <- min(t2[,1])
                xmax <- max(t2[,1])
                ymin <- min(t2[,5])
                ymax <- max(t2[,5])
                g <- ggplot(data=t2, aes(x=t2$`Story points`, y=t2$`AEE Pounded`), environment = e)
                g <- g + ggtitle("Relation of (Miss)estimation with story points")
                g <- g + geom_bar(stat='identity', aes(fill=t2$`Aggregated estimate (h)`))
                g <- g + scale_fill_continuous(name="Estimate (h)")
                g <- g + ylab(expression(frac(sum("Estimate error (h)"),"Amount of issues")))
                g <- g + xlab("Story points")
                g <- g + annotation_custom(tableGrob(t1),
                                           xmax=xmax,
                                           ymin=ymin,
                                           xmin=xmin + (xmax-xmin)/8,
                                           ymax=ymin + (ymax-ymin)/8)
                print(g)
            })

            issuesOfUser <- reactive({
                ofUser <- grep(input$selectTeamMember, issuesDF$'Assignee key')
                withStatus <- grep(gsub(", ", "|", toString(input$checkboxgroupIssueStatus)), issuesDF[,"Status"])
                toShow <- intersect(ofUser, withStatus)
                ## cat(file=stderr(), "withStatus: ", as.character(withStatus), "\n")
                output$debug <- renderPrint ({ toShow })
                issuesDF[toShow,]
            })
            
            output$table <- renderTable({
                issuesOfUser()
            })

            ## The story points are in: $issues$fields$customfield_10008
            ## We use them only in the /stories/ not in the /sub-tasks/
            ## The /timespent/ and the /originalestimate/ are only present
            ## in the sub-task
            ## =>
            ## You have to look for the story points associated to a subtask
            ## in the parent of the subtask which is a story
            
            histdata <- rnorm(500)

            output$teamBurn <- renderPlot({
                sprintTotalSeconds <- sum(issuesDF[!is.na(issuesDF[,'Estimate (h)']),'Estimate (h)']) * 3600
                data <- NULL
                ##    data <- allWorklog(issuesJson, sprintStart, sprintEnd)
                data <- allWorklog(awl, sprintStart, sprintEnd)
                data <- data[order(data[,"started"]),]
                data <- group_by(data, `started`)
                data2 <- summarize(data, sumDay=sum(`spent`))
                data2 <- cbind(data2, rest=sprintTotalSeconds - Reduce("sum", unlist(data2[,2]), accumulate=TRUE))
                e <- environment()
                g <- ggplot(data=data2, aes(x=started, y=rest/3600),environment = e)
                ##g <- g + geom_point()
                ##g <- g + geom_line()
                ##g <- g + geom_bar(stat='identity')
                g <- g + geom_area(alpha = 0.7, fill='steelblue')
                g <- g + ylab("Totla time - Time spent (h)")
                g <- g + xlab("Date")
                g <- g + ggtitle("Burning of team")
                print(g)
            })

            output$worklog <- renderPlot({
                data <- worklogOfAssignee(input$selectTeamMember, issuesJson, awl, sprintStart, sprintEnd)
                ## message("data\n", nrow(data)," ",ncol(data)," ",names(data),"\n",data[1,])
                e <- environment()
                ymax<- max(data[,3])/3600
                g <- ggplot(environment = e)
                g <- g + geom_bar(data=data
                                 ,aes(x=started+(86400/2)
                                     ,y=(spent/3600)
                                     ,alpha=.5
                                     ,width=86400
                                      )
                                 ,stat="identity"
                                 ,fill="steelblue"
                                  )
                ## g <- g + geom_text(data=data
                ##                    ,aes(label=data[,1]
                ##                         ,x=started
                ##                         ,y=(spent/3600)*2)
                ##                    ,hjust=2
                ##                    ,stat="identity"
                ##                    ,angle=90
                ##                    ##,size=(data[,3]/3600)*(5/8)
                ##                    )
                for (d in dates[weekend]){
                    xmin <- as.POSIXct(strptime(as.Date(d, origin = "1970-01-01"),
                                                format="%Y-%m-%d"), origin = "1970-01-01",
                                       tz="America/Mexico_City")
                    xmax <- xmin + 86400
                    g <- g + annotate("rect", xmin=xmin, xmax=xmax, ymin=0, ymax=ymax, alpha=0.3)
                }
                g <- g + theme(legend.position="none")
                g <- g + ggtitle(paste0("Spent of ", usersDF[usersDF[,1]==input$selectTeamMember,2]))
                g <- g + ylab("Hours")
                g <- g + scale_x_datetime(date_labels = "%b %d", date_breaks="1 day")
                g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1))
                print(g)
            })

            output$personalBurn <- renderPlot({
                assignee <- input$selectTeamMember
                sprintTotalSeconds <- sum(issuesDF[!is.na(issuesDF[,'Estimate (h)'])&issuesDF[,2]==assignee,'Estimate (h)']) * 3600
                data <- NULL
                data <- worklogOfAssignee(assignee, issuesJson, awl, sprintStart, sprintEnd)
                data <- data[order(data[,"started"]),]
                data <- group_by(data, `started`)
                data2 <- summarize(data, sumDay=sum(`spent`))
                data2 <- cbind(data2, rest=sprintTotalSeconds - Reduce("sum", unlist(data2[,2]), accumulate=TRUE))
                e <- environment()
                g <- ggplot(data=data2, aes(x=started, y=rest/3600),environment = e)
                ##g <- g + geom_point()
                ##g <- g + geom_line()
                ##g <- g + geom_bar(stat='identity')
                g <- g + geom_area(alpha = 0.7, fill='steelblue')
                g <- g + ylab("Totla time - Time spent (h)")
                g <- g + xlab("Date")
                g <- g + ggtitle(paste0("Burning of ",usersDF[usersDF[,1]==input$selectTeamMember,2]))
                print(g)
            })

            output$originalVSspent <- renderPlot({
                data <- NULL
                ofUser <- grep(input$selectTeamMember, issuesDF$'Assignee key')
                withStatus <- grep(gsub(", ", "|", toString(input$checkboxgroupIssueStatus)), issuesDF[,"Status"])
                toShow <- intersect(ofUser, withStatus)
                data <- issuesDF[toShow,c("Issue key", "Estimate (h)", "Spent (h)")]
                data <- data[!is.na(data[,"Estimate (h)"]) & !is.na(data[,"Spent (h)"]),]
                data <- melt(data)
                e <- environment()
                g <- ggplot(environment = e)
                g <- g + geom_bar(data=data
                                 ,aes(x=reorder(`Issue key`, `value`)
                                      ##x=`Issue key`
                                     ,y=`value`
                                     ,fill=variable
                                      )
                                 ,stat="identity"
                                 ,position=position_dodge()
                                  )
                g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1))
                g <- g + ylab("Time (h)")
                g <- g + xlab("Issue key")
                g <- g + ggtitle(paste0("Estimations of ",usersDF[usersDF[,1]==input$selectTeamMember,2]))
                print(g)
            })

            output$plot1 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
            })

            output$estimationError <- renderPlot({
                x <- aggregate(cbind(`Story points`,`Estimate (h)`,`Spent (h)`,`Estimate error (h)`) ~ `Assignee name`, issuesDF, sum)
                e <- environment()
                g <- ggplot(environment = e)
                g <- g + ggtitle("Estimation errors. Negative means more work than estimated")
                g <- g+geom_bar(data=x,
                                aes(x=reorder(x$`Assignee name`,
                                              x$`Estimate error (h)`),
                                    y=x$`Estimate error (h)`,
                                    fill=x$`Assignee name`),
                                stat='identity')
                g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1))
                g <- g + ylab("Estimate error (h)")
                g <- g + xlab("")
                g <- g + guides(fill = FALSE)
                print(g)
            })

            output$page <- renderUI({
                ## To disappear the login block
                ##     ## box(width = 12, div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Contratulations you got in!",ui2()))))
            })
        }
    })

    ## output$selectTeamMemberValue <- renderPrint({ input$selectTeamMember })
    ## output$checkboxgroupIssueStatusValue <- renderPrint({ input$checkboxgroupIssueStatus })

    ## message("Original estimate:\n",summarize(group_by(i[!is.na(i$`original estimate (h)`),],`assignee name`), count=n(),sum(`original estimate (h)`)))
    ## message("Time spent:\n",summarize(group_by(i[!is.na(i$`time spent (h)`),],`assignee name`), count=n(),sum(`time spent (h)`)))
}

shinyApp(ui, server)
