#' @title Issues download and processing.



#' Downloads via the REST API of jira the json of jira users.
#'
#' @return A json with the issues of a sprint
#' @export
getIssuesJson <- function() {
    t1 <- proc.time()
    if(canPingJira()) {
        r <- POST(paste0(jiraIp, ":", jiraPort, "/rest/api/2/search"), authenticate(Username, Password, "basic"), add_headers("Content-Type" = "application/json"), verbose(), body = paste0("{\"jql\":\"Sprint = ",sprintId,"\",\"maxResults\":1500}"))
        ##    r <- POST(paste0(jiraIp,":",jiraPort,"/rest/api/2/search"), authenticate(username, password, "basic"), add_headers("Content-Type" = "application/json"), verbose(), body = "{\"jql\":\"Sprint in openSprints() AND Sprint not in (closedSprints(), futureSprints())\",\"maxResults\":1500}")
        issues <- fromJSON(content(r,"text"))
        save(issues, file="jiradata/issues")
    } else {
        load("jiradata/issues")
    }
    cat(file=stderr(), "a) Time spent: ", proc.time()-t1, "\n")
    issues
}

#' Creates a matrix with the issues from a list.
#'
#' @param issuesKeys A list of factors with the keys of the issues
#' @return matrix with 7 columns "issue", "assignee", "expand", "id", "self", "field".  The column "assignee" is the key. The column "fields" is the original json from the issue.
#' @export
getIndividualIssuesJson <- function(issueKeys) {
    t1 <- proc.time()
    if(canPingJira()) {
        issuesIndividualList <- NULL
        for (i in issueKeys) {
            message("Downloading: ",i,"\n")
            issue <- GET(paste0("http://",jiraIp,":",jiraPort,"/rest/api/2/issue/",i), authenticate(Username, Password, "basic"), add_headers("Content-Type" = "application/json"))
            issue <- fromJSON(content(issue, "text"))
            save(issue, file=paste0("jiradata/issuesIndividual/",i))
            assign(i,issue)
            issuesIndividualList <- rbind(issuesIndividualList, c(issue=i, assignee=issue$fields$assignee$key, issue))
        }
        save(issuesIndividualList, file="jiradata/issuesIndividualList")
    } else {
        load("jiradata/issuesIndividualList")
    }
    cat(file=stderr(), "b) Time spent: ", proc.time()-t1, "\n")
    message("Took (seg):     ", proc.time() - t1, "\n")
    issuesIndividualList
}

#' Creates a data frame with basic data of the sprint's users.
#'
#' @param issues A list with the json of the issues in the sprint
#' @return Data frame with columns "Issue key", "assignee key", "assignee name", "status", "type", "original estimate (h)", "time spent (h)", "difference (h)", "story points", "story points filled", "parent", "time judgement"
#' @export
buildDFissues <- function(issues) {
    ## Time spent by issue in this sprint
    wl <- issueListWorklog(issues$issues$key, awl, sprintStart, sprintEnd)
    ## Aggregate by issue (every worklog is a row)
    timeSpentByIssue <- aggregate(spent ~ `Issue key`, wl, sum)
    ## Build a data.frame to make all the calculations
    ## Clean the people without issues
    logicVectorOfIssuesWithLog <- issues$issues$key %in% timeSpentByIssue$`Issue key`
    t <- as.data.frame(issues$issues$key[logicVectorOfIssuesWithLog])
    names(t) <- "Issue key"
    t[,"assignee key"] <- issues$issues$fields$assigne$key[logicVectorOfIssuesWithLog]
    t[,"assignee name"] <- issues$issues$fields$assigne$displayName[logicVectorOfIssuesWithLog]
    t[,"status"] <- issues$issues$fields$status$name[logicVectorOfIssuesWithLog]
    t[,"type"] <- issues$issues$fields$issuetype$name[logicVectorOfIssuesWithLog]
    t[,"original estimate (h)"] <- (issues$issues$fields$timeoriginalestimate[logicVectorOfIssuesWithLog])/3600
    ## This *time spent* takes into account all the time spent in the issue
    ## disregard of the sprint in which was that time spent
    ## t[,"time spent (h)"] <- (issues$issues$fields$timespent[issues$issues$key %in% timeSpentByIssue$`Issue key`])/3600
    ## To be adecuate for us, it has to be calculated from the worklogs
    t <- merge(t,timeSpentByIssue, by="Issue key")
    colnames(t)[7] <- "time spent (h)"
    t[,"time spent (h)"] <- t[,"time spent (h)"]/3600
    t[,"difference (h)"] <- t[,"original estimate (h)"] - t[,"time spent (h)"]
    t[,"story points"] <- issues$issues$fields$customfield_10008[issues$issues$key %in% timeSpentByIssue$`Issue key`]
    ##    t[,"story points filled"] <- setStoryPoints(issues[issues$issues$key %in% timeSpentByIssue$`Issue key`])
    t[,"parent"] <- issues$issues$fields$parent$key[issues$issues$key %in% timeSpentByIssue$`Issue key`]
    ## Maximum story points in the sprint
    maxSP <- max(as.integer(issues$issues$fields$customfield_10008)[!is.na(as.integer(issues$issues$fields$customfield_10008))])
    ##t[,"time judgement"] <- (t[,"time spent (h)"]/t[,"original estimate (h)"]) * (t[,"story points filled"]/maxSP)
    message(paste0("nrow ",nrow(t)))
    message(paste0("nrcol ", ncol(t)))
    return (t)
}

#' Creates a data frame for analyzing the issues gotten in the json.
#'
#' @param issues A list with the json of the issues in the sprint
#' @return Data frame with columns "Issue key", "assignee key", "assignee name", "status", "type", "original estimate (h)", "time spent (h)", "difference (h)", "story points", "story points filled", "parent", "time judgement"
#' @export
buildDFAnalysisIssues <-function() {
    allIssues <- i1$issues$key
    t <- as.data.frame(allIssues)
    t[,"Summary"]                 <- substr(issueSummary(allIssues),1,15)
    t[,"Type"]                    <- issueType(allIssues)
    t[,"Type"]                    <- as.factor(t[,"Type"])
    t[,"Assignee key"]            <- issueAssigneeKey(allIssues)
    t[,"Assignee name"]           <- issueAssigneeDisplayName(allIssues)
    t[,"Assignee name"]           <- as.factor(t[,"Assignee name"])
    t[,"Story points"]            <- issueStoryPoints(allIssues)
    t[,"Status"]                  <- issueStatus(allIssues)
    t[,"Status"]                  <- as.factor(t[,"Status"])
    t[,"Estimate (h)"]            <- issueTimeOriginalEstimate(allIssues)/3600
    t[,"Spent (h)"]               <- issueTimeSpent(allIssues)/3600
    t[,"# Sprints"]               <- issueSprintsAmount(allIssues)
    t[,"Aggregated estimate (h)"] <- issueAggregateTimeOriginalEstimate(allIssues)/3600
    t[,"Aggregated spent (h)"]    <- issueAggregatedProgress.Total(allIssues)/3600
    names(t)[1] <- c("Issue key")
    t <- mutate(t, `Estimate error (h)` = `Estimate (h)` - `Spent (h)`)
    t <- mutate(t, `Aggregated estimate error (h)` = `Aggregated estimate (h)` - `Aggregated spent (h)`)
    t[is.na(t)] <- 0
    message(paste0("nrow ",nrow(t)))
    message(paste0("nrcol ", ncol(t)))
    return (t)
}

## Basic issue functions

#' Gets the logic vector where a list of issuekeys are in the json of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @examples
#' issueRow("PGR-64")
#' issueRow(c("PGR-64","VRT-60"))
#' @return Logic vector identifing the position of the issues in
#'   the json of issues.
#' @export
issueRow <- function(issueKey) {
    i1$issues$key %in% `issueKey`
}

#' Gets the parent of a list of issue keys.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of characters with the keys of the parent of the issueKey
#'   If does not have parent returns \code{NA}
#' @examples
#' issueRow("PGR-64")
#' issueRow(c("PGR-64","VRT-60"))
#' @export
issueParent <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"parent"][,"key"]
}

#' Gets the children of an issue in a character vector.
#' 
#' If does not have children returns `NULL`
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of strings with the keys of the children of the issueKey
#' @examples
#' issueChildren("VRT-81")
#' @export
issueChildren <- function(issueKey) {
    if (length(issueKey) > 1) { stop ("This function accepts only one issue as input.") }
    i1$issues$fields[which(issueRow(`issueKey`)),"subtasks"][[1]]$key
}

#' Gets the type of the list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Data frame of the issue types of the issueKeys
#' @examples
#' issueType("PGR-64")
#' issueType(c("PGR-64","VRT-60"))
#' @export
issueType <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"issuetype"]["name"]
}

#' Gets the assignee key of the list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Data frame of the assignee keys of the issueKeys
#' @examples
#' issueAssigneeKey("PGR-64")
#' issueAssigneeKey(c("PGR-64","VRT-60"))
#' @export
issueAssigneeKey <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"assignee"]["key"]
}

#' Gets the display name of the assignee of the list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Data frame of the display name of the assignee keys of the issueKeys
#' @examples
#' issueAssigneeDisplayName("PGR-64")
#' issueAssigneeDisplayName(c("PGR-64","VRT-60"))
#' @export
issueAssigneeDisplayName <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"assignee"]["displayName"]
}

#' Gets the story points of a list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of numeric with the story points of the issueKeys
#'   If does not have story points returns \code{NA}
#' @examples
#' issueStoryPoints("PGR-64")
#' issueStoryPoints(c("PGR-64","VRT-60"))
#' @export
issueStoryPoints <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"customfield_10008"]
}

#' Gets the status of a list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of character with the status of an issueKey
#' @export
issueStatus <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"status"][,"name"]
}

#' Gets the \strong{summary} of a list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of character with the status of an issueKey
#' @export
issueSummary <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"summary"]
}

#' Gets the timeoriginalestimate for a list of issues.
#'
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of numeric with the sum of the estimated times
#'   for an issue and it's children issues.
#'   It returns \code{NA} if the issue has subtasks because the field
#'   \strong{aggregatetimeoriginalestimate} of an issue with subtasks
#'   sums the estimated time from the subtasks.
#'   Also returns \code{NA} if the issue has no parent but the original
#'   estimate was not established.
#' @export
issueTimeOriginalEstimate <- function(issueKey) {
    i1$issues$fields[which(issueRow(issueKey)), "timeoriginalestimate"]
}

#' Gets the \strong{timespent} in a list of issues.
#'
#' It returns \code{NA} if the issue has subtasks because the field
#' \strong{total} of an issue with subtasks sums the logged time
#' from the subtasks.
#' @param issueKey A string with the issue key
#' @return Numeric with the sum of the logged work of an issue
#' @export
issueTimeSpent <- function(issueKey) {
    i1$issues$fields[which(issueRow(issueKey)), "timespent"]
}

#' Gets the \strong{aggregatetimeoriginalestimate} in a list of issues.
#'
#' @param issueKey A string with the issue key
#' @return Numeric with the sum of the logged work of an issue and
#'   the children of this
#' @export
issueAggregateTimeOriginalEstimate <- function(issueKey) {
    i1$issues$fields[which(issueRow(issueKey)), "aggregatetimeoriginalestimate"]
}

#' Gets the \strong{aggregateprogress.total} of a list of issues.
#' 
#' @param issueKey A vector of characters with the issue keys
#' @return Vector numeric whit the aggregated logged work
#' @export
issueAggregatedProgress.Total <- function(issueKey) {
    i1$issues$fields[which(issueRow(`issueKey`)),"aggregateprogress"][,"total"]
}

#' Gets the amount of sprints in which the issue list was not yet DONE.
#'
#' @param issueKey A vector of characters with the issue keys
#' @return Vector of integer with the number of sprints for each issue.
#' @export
issueSprintsAmount <- function(issueKey) {
    sapply(i1$issues$fields[which(issueRow(issueKey)), "customfield_10000"], length)
}

## In i1$issues$fields[134,"customfield_10000"]
## there are the sprints in which the issue lived
## Output example:
## [[1]]
## [1] "com.atlassian.greenhopper.service.sprint.Sprint@461b58ce[rapidViewId=21,state=CLOSED,name=Sprint 25 - Frijoles charros,startDate=2016-09-28T16:30:00.000-05:00,endDate=2016-10-18T21:00:00.000-05:00,completeDate=2016-10-18T19:22:19.788-05:00,sequence=48,id=48]"
## [2] "com.atlassian.greenhopper.service.sprint.Sprint@63d29d9e[rapidViewId=21,state=CLOSED,name=Sprint 26 - Guacamole,startDate=2016-10-19T15:00:00.000-05:00,endDate=2016-11-08T18:00:00.000-06:00,completeDate=2016-11-09T08:58:36.968-06:00,sequence=49,id=49]"       
## [3] "com.atlassian.greenhopper.service.sprint.Sprint@7a7e0819[rapidViewId=21,state=ACTIVE,name=Sprint 27 - HORCHATA,startDate=2016-11-09T11:53:40.613-06:00,endDate=2016-11-29T18:00:00.000-06:00,completeDate=<null>,sequence=50,id=50]"                               
## A possible measure for estimation or unexpected circumstances. If an issue changes from sprint something "bad" should have happend.

####
## TESTS
####

## t1<-proc.time()
## for (us in usersDF$u.key){
##   x<-get(us)
##   if(length(x)>0){
##     message (us);
##     message("Hours: ", sum(x[,3])/3600)
##   }
## }
## proc.time() - t1

###################
## ricardo.olvera
###################

## dates <- as.Date(ricardo.olvera[1,2]) + 0:21
## weekend <- weekdays(ricardo.olvera[,2]) %in% c("Saturday", "Sunday")
## ymax<- max(ricardo.olvera[,3])/3600
## g <- ggplot()
## g <- g + geom_bar(data=ricardo.olvera,
##                   aes(started, spent/3600, alpha=.5),
##                   stat="identity", fill="steelblue", width=22400)
## for (d in ricardo.olvera[weekend, 2]){
##   xmax <- as.POSIXct(d, origin = "1970-01-01") + 86400
##   xmin <- as.POSIXct(d, origin = "1970-01-01")
##   g <- g + geom_rect(alpha=0.2, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax))
## }
## g <- g + ylab("Hours")
## g <- g + ggtitle(usersDF[usersDF[,1]=="ricardo.olvera",2])
## g <- g + theme(legend.position='none')
## g


## ###################
## dates <- as.Date(ricardo.olvera[1,2]) + 0:21
## weekend <- weekdays(dates) %in% c("Saturday", "Sunday")
## ymax<- max(ricardo.olvera[,3])/3600
## g <- ggplot()
## g <- g + geom_bar(data=ricardo.olvera
##                   ,aes(started
## 		       ,spent/3600
## 		       ,alpha=.5
## 		       ,width=spent
## 		       )
## 		  ,stat="identity"
## 		  ,fill="steelblue"
## 		  )
## g <- g + geom_text(data=ricardo.olvera
## 		   ,aes(label=as.character(ricardo.olvera[,1])
## 					  ,x=started
## 					  ,y=spent/3600)
## 		   ,hjust=2
## 		   ,stat="identity"
## 		   ,angle=90
## 		   ,size=(ricardo.olvera[,3]/3600)*(5/8)
## 		   )
## for (d in dates[weekend]){
##   xmin <- as.POSIXct(strptime(as.Date(d, origin = "1970-01-01"), format="%Y-%m-%d"), origin = "1970-01-01", tz="America/Mexico_City")
##   xmax <- xmin + 86400
##   g <- g + annotate("rect", xmin=xmin, xmax=xmax, ymin=0, ymax=ymax, alpha=0.3)
## }
## g <- g + theme(legend.position="none")
## g <- g + ggtitle(usersDF[usersDF[,1]=="ricardo.olvera",2])
## g <- g + ylab("Hours")
## g

## ####################
## ## everybody
## ####################
## u2<-usersDF[1:2,1]
## dates <- as.Date(ricardo.olvera[1,2]) + 0:21
## weekend <- weekdays(dates) %in% c("Saturday", "Sunday")
## g <- ggplot()
## ymax <- 0
## ##for (us in usersDF[,1]){
## for (us in u2){
##   data <- get(us)
##   if(!is.null(data)){
##     message(us)
##     if(max(data[,3])/3600 > ymax) ymax <- max(data[,3])/3600
##     message(length(data[,1]))
##     message(length(data[,2]))
##     message(length(data[,3]))
##     g <- g + geom_bar(data=data
##                       ,aes(started
##                            ,spent/3600
##                            ,alpha=.5
##                            ,width=spent
##                            )
##                       ,stat="identity"
##                       ,fill="steelblue"
##                       )
##     g <- g + geom_text(data=data
##                        ,aes(label=data[,1]
##                             ,x=started
##                             ,y=spent/3600)
##                        )
##     ## g <- g + geom_text(data=data
##     ##                    ,aes(label=data[,1]
##     ##                         ,x=started
##     ##                         ,y=spent/3600)
##     ##                    ,hjust=2
##     ##                    ,stat="identity"
##     ##                    ,angle=90
##     ##                    ,size=(data[,3]/3600)*(5/8)
##     ##                    )
##   }
## }
## for (d in dates[weekend]){
##   xmin <- as.POSIXct(strptime(as.Date(d, origin = "1970-01-01")
##                               ,format="%Y-%m-%d")
##                      ,origin = "1970-01-01"
##                      ,tz="America/Mexico_City")
##   xmax <- xmin + 86400
##   g <- g + annotate("rect", xmin=xmin, xmax=xmax, ymin=0, ymax=ymax, alpha=0.3)
## }
## g <- g + ylab("Hours")
## g

## scale_fill_manual(values = alpha(c("blue", "red"), .3))
