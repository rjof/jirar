#' @title Individual worklog download and processing.

#' Creates a matrix with the issues from a list.
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

#' Gets the worklogs of a issue list keys.
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

## Basic worklog functions

#' Gets the logic vector where a issuekey is in the matrix of worklogs
#' @param issueKey A string with the issue key
#' @examples
#' worklogRow("PGR-64")
#' @export
worklogRow <- function(issueKey) {
    awl[,"issue"] == `issueKey`
}

