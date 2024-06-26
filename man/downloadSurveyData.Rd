% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{downloadSurveyData}
\alias{downloadSurveyData}
\title{Download Dataset from Delosis Limesurvey server}
\usage{
downloadSurveyData(
  surveyID,
  sDocumentType = "csv",
  sLanguageCode = "en",
  sCompletionStatus = "all",
  sHeadingType = "code",
  sResponseType = "short"
)
}
\arguments{
\item{surveyID}{ID of survey}

\item{sDocumentType}{document type default:csv}

\item{sLanguageCode}{language default:en}

\item{sCompletionStatus}{completionStatus default:all}

\item{sHeadingType}{heading type default:code}

\item{sResponseType}{response type default:short}
}
\description{
Download Dataset from Delosis Limesurvey server
}
