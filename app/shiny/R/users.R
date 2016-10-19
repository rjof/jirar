#' @title Get the users in the jira instance.
#'

#' Retrives the users from Jira.
#' @export
getUsers <- function() {
    message("Username ", Username)
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
