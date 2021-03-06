% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/issues.R
\name{getAllWorklogs}
\alias{getAllWorklogs}
\title{Creates a matrix with the issues from a list.}
\usage{
getAllWorklogs(issueKeys)
}
\arguments{
\item{issuesKeys}{A list of factors with the keys of the issues}
}
\value{
Data frame with columns "Issue key", "assignee key", "assignee name", "status", "type", "original estimate (h)", "time spent (h)", "difference (h)", "story points", "story points filled", "parent", "time judgement"
}
\description{
Creates a matrix with the issues from a list.
}

