#' @title Issues download and processing.



#' Get the json of jira users.
#' @export
#'
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
#' @return  matrix with 7 columns "issue", "assignee", "expand", "id", "self", "field".  The column "assignee" is the key. The column "fields" is the original json from the issue.
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

#' Creates a matrix with the issues from a list.
#'
#' @param issuesKeys A list of factors with the keys of the issues
#' @return Data frame with columns "Issue key", "assignee key", "assignee name", "status", "type", "original estimate (h)", "time spent (h)", "difference (h)", "story points", "story points filled", "parent", "time judgement"
#' @export
getAllWorklogs <- function(issueKeys) {
  worklogIndividualList <- NULL
  t1 <- proc.time()
  if(canPingJira()) {
    for (i in issueKeys) {
      message("Downloading worklog-------------------: ",i,"\n")
      worklog <- GET(paste0("http://",jiraIp,":",jiraPort,"/rest/api/2/issue/",i,"/worklog"), authenticate(Username, Password, "basic"), add_headers("Content-Type" = "application/json"))
      worklog <- fromJSON(content(worklog, "text"))
      save(worklog, file=paste0("jiradata/worklogsIndividual/",i))
      assign(i,worklog)
      message(worklog$fields$assignee$key)
      worklogIndividualList <- rbind(worklogIndividualList, c(issue=i, assignee=worklog$fields$assignee$key, worklog))
    }
    save(worklogIndividualList, file="jiradata/worklogIndividualList")
  } else {
    load(file="jiradata/worklogIndividualList")
  }
  message("Time spent: ")
  message(c(proc.time()-t1), "\n")
  worklogIndividualList
}

#' Creates a data frame with basic data of the sprint's users.
#'
#' @param issues A list with the json of the issues in the sprint
#' @return Data frame with columns "Issue key", "assignee key", "assignee name", "status", "type", "original estimate (h)", "time spent (h)", "difference (h)", "story points", "story points filled", "parent", "time judgement"
#' @export
buildDFissues <- function(issues) {
  ## Time spent by issue in this sprint
  ## wl <- issueListWorklog(issues$issues$key, awl, sprintStart, sprintEnd)
  ## Aggregate by issue (every worklog is a row)
  ## timeSpentByIssue <- aggregate(spent ~ `Issue key`, wl, sum)
  ## Build a data.frame to make all the calculations
  ## Clean the people without issues
  t <- as.data.frame(issues$issues$key)
  names(t) <- "Issue key"
  t[,"assignee key"] <- issues$issues$fields$assigne$key
  t[,"assignee name"] <- issues$issues$fields$assigne$displayName
  t[,"status"] <- issues$issues$fields$status$name
  t[,"type"] <- issues$issues$fields$issuetype$name
  t[,"original estimate (h)"] <- issues$issues$fields$timeoriginalestimate/3600
  ## This *time spent* takes into account all the time spent in the issue
  ## disregard of the sprint in which was that time spent
  t[,"time spent (h)"] <- issues$issues$fields$timespent/3600
  ## To be adecuate for us, it has to be calculated from the worklogs
  ##t[,"time spent (h)"] <- timeSpentByIssue/3600
  t[,"difference (h)"] <- t[,"original estimate (h)"] - t[,"time spent (h)"]
  t[,"story points"] <- issues$issues$fields$customfield_10008
  t[,"story points filled"] <- setStoryPoints(issues)
  t[,"parent"] <- issues$issues$fields$parent$key  
  ## Maximum story points in the sprint
  maxSP <- max(as.integer(issues$issues$fields$customfield_10008)[!is.na(as.integer(issues$issues$fields$customfield_10008))])
  ##t[,"time judgement"] <- (t[,"time spent (h)"]/t[,"original estimate (h)"]) * (t[,"story points filled"]/maxSP)
  message(paste0("nrow ",nrow(t)))
  message(paste0("nrcol ", ncol(t)))
  return (t)
}

#' Remove from the users.
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
removeEmptyUsers <- function(issuesDF, usersDF) {
  newUsersDF <- data.frame()
  for (user in usersDF[,'u.key']){
    if(nrow(issuesDF[issuesDF[,'assignee key'] == user,]) > 0){
      newUsersDF <- rbind(newUsersDF, usersDF[usersDF[,'u.key'] == user,])
    }
  }
  newUsersDF
}

#' Gets the worklogs of a issue list keys.
#'
#' @param issueList A vector with keys of lists
#' @param worklogs A matrix with all the worklogs of the sprint
#' @examples
#' issueListWorklog(c("PGR-64", "CADMF-219", "BDATA-19"), awl, sprintStart, sprintEnd)
#' @export
issueListWorklog <- function(issueList, worklogs, start, end) {
  ## message("Is awl comming right ", class(awl))
  answer <- NULL
  inTheIssueList <- worklogs[,1] %in% intersect(worklogs[,1],issueList)
  withEntries <- worklogs[,3] > 0
  df1 <- NULL
  tmp <- worklogs[inTheIssueList & withEntries,]
  for (i in 1:length(tmp[,1])){
    for (j in 1:length(tmp[i,5]$worklogs$started)) {
      if(tmp[i,5]$worklogs$started[j] >= start
         &
         tmp[i,5]$worklogs$started[j] <= end){
        df1 <- rbind(df1, c(
                            as.character(unlist(tmp[i,1]))
                            ,started=tmp[i,5]$worklogs$started[j]
                            ,spent=as.integer(tmp[i,5]$worklogs$timeSpentSeconds[j])))
      }
    }
  }
  df1 <- as.data.frame(df1)
  names(df1) <- c("Issue key", "Date", "spent")
  df1[,1] <- as.character(df1[,1])
  df1[,2] <- as.POSIXct(df1[,2], format ="%Y-%m-%dT%H:%M:%S")#origin = "1970-01-01 0:0:0")
  df1[,3] <- as.numeric(as.character(df1[,3]))
  df1
}


#' Returns the worklog of a user.
#' @export
worklogOfAssignee <- function(assigneeKey, issuesMatrix, worklogsMatrix, start, end){
  ###############################################################################
  ## Follow rjof
  ## Why is different the worklog$worklogs$started in the issue and in the worklog
  ## For row 61 there are 20 logs in issues and 23 in worklogs
  ###############################################################################
  worklogs <- data.frame()
  tmp <- unlist(issuesMatrix[issuesMatrix[,'assignee']==assigneeKey,"issue"])
  m2 <- cbind(tmp,worklogsMatrix[unlist(worklogsMatrix[,"issue"]) %in% tmp,"worklogs"])
  c1 <- NULL
  c2 <- NULL
  c3 <- NULL
  for (i in 1:length(tmp)){
    if(length(m2[,2][[i]]) > 0){
      c1 <- c(as.vector(c1), as.vector(rep(tmp[i],length(unlist(m2[,2][[i]]['started'])))))
      c2 <- c(c2, as.POSIXct(strptime(as.character(unlist(m2[,2][[i]]['started'])),
                                      format="%Y-%m-%d"),
                             origin = "1970-01-01", tz="America/Mexico_City"))
##      c2 <- c(as.vector(c2),as.vector(as.POSIXct(strptime(as.character(unlist(m2[,2][[i]]['started'])),format="%Y-%m-%d"),origin = "1970-01-01", tz="America/Mexico_City")))
##      c2 <- c(as.vector(c2),as.vector(unlist(m2[,2][[i]]['started'])))
      c3 <- c(as.vector(c3), as.vector(unlist(m2[,2][[i]]['timeSpentSeconds'])))
    }
  }
  worklogs <- as.data.frame(cbind(key=c1, started=c2, spent=c3, deparse.level=0))
  if(nrow(worklogs) > 0){
    worklogs[,2] <- as.POSIXct(as.numeric(as.character(worklogs[,2])),format="%Y-%m-%d", origin = "1970-01-01", tz="America/Mexico_City")
    worklogs[,3] <- as.numeric(as.character(worklogs[,3]))
    ## Filter sprint dates
    worklogs <- worklogs[as.Date(worklogs[,'started']) >= start,]
    worklogs <- worklogs[as.Date(worklogs[,'started']) <= end,]
    return(worklogs)
  } else {
    return(NULL)
  }
}
##x<-worklogOfAssignee("emilio.mendez", issuesJson, awl, sprintStart, sprintEnd)

#' Returns the worklog of all the users.
#' @export
allWorklogViejo <- function(worklogsMatrix, start, end){
  worklogs <- data.frame()
  for (i in 1:nrow(worklogsMatrix)){
    if (unlist(worklogsMatrix[i,"total"]) > 0) {
      for(j in 1:unlist(worklogsMatrix[i,"total"])){
        if(worklogsMatrix[i,"worklogs"]$worklogs$started[j] >= start
           &
           worklogsMatrix[i,"worklogs"]$worklogs$started[j] <= end){
          x <- data.frame(key=worklogsMatrix[i,1],
                          started=as.POSIXct(strptime(as.character(worklogsMatrix[i,"worklogs"]$worklogs$started[j])
                            ,format="%Y-%m-%d")
                            ,origin = "1970-01-01", tz="America/Mexico_City")
                          ,spent=worklogsMatrix[i,"worklogs"]$worklogs$timeSpentSeconds[j])
        }
        worklogs <- rbind(worklogs, x)
      }
    }
  }
  if(length(worklogs) > 0){
    worklogs[,2] <- as.POSIXct(worklogs[,2])
    return(worklogs)
  } else {
    return(NULL)
  }
}
##a2<-allWorklog(awl, sprintStart, sprintEnd)

#' R style wich returns the worklog of all the users.
#' @export
allWorklog <- function(worklogsMatrix, start, end) {
    worklogsList<-worklogsMatrix[,5]
    allStarted <- lapply(worklogsList, function(e){ e$started })
    allLoggedTime <- lapply(worklogsList, function(e){ e$timeSpentSeconds })
    listaIssues <- worklogsMatrix[,1]
    tmp <- lapply(allStarted,data.frame)
    issuesList <- lapply(tmp, function(e){
        list(rep(listaIssues[match(list(e), tmp)], nrow(e)))
    })
    out <- data.frame(unlist(issuesList), unlist(allStarted, recursive=T), unlist(allLoggedTime))
    out[,2] <- as.POSIXct(out[,2])
    names(out) <- c("issue", "started", "spent")
    out
}
##a3<-allWorklog2(awl, sprintStart, sprintEnd)

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
