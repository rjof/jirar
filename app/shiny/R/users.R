#' @title Get the users in the jira instance.
#'

#' Retrives the users from Jira.
#' @export
getUsers <- function() {
    if (canPingJira()) {
        u1 <- GET(paste0("http://",jiraIp,":",jiraPort,"/rest/api/2/user/assignable/search?project=",jiraProject), authenticate(Username, Password, "basic"), add_headers("Content-Type" = "application/json"), verbose())
        users <- fromJSON(content(u1, "text"))
        save(users, file="jiradata/users")
    } else {
        load("jiradata/users")
    }
    users
}

#' Returns a vector with key and displayName of users.
#' @export
getUserChoices <- function(u) {
    u2 <- u$u.key
    names(u2) <- u$u.displayName
    return(u2)
}

#' Returns a data.frame with key and displayName of users.
#' @export
getUserDF <- function(u) {
    u2 <- data.frame(u$key,
                     u$displayName,
                     stringsAsFactors=FALSE)
    u2
}

#' Remove from the users.
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
        if(nrow(issuesDF[issuesDF[,'Assignee key'] == user,]) > 0){
            newUsersDF <- rbind(newUsersDF, usersDF[usersDF[,'u.key'] == user,])
        }
    }
    newUsersDF
}
