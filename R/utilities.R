# Additional utility functions for derivative generation
#
#  Copyright (C) 2017-2019 Delosis
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' Select Iteration from dataset
#'
#' Requires \code{User.code}, \code{Iteration}, \code{Completed}
#' Optionally \code{Valid}
#' columns in input data frame.
#'
#' By default returns the df trimmed to the first complete iteration for each user code
#'
#' @param df Data frame with simple questionnaire, read from CSV file exported from Delosis server.
#' @param iterationFunction function to apply to Iteration - default min
#' @param completed restrict to completed Iterations only - default TRUE
#' @param valid restrict to Iterations marked as valid IF the user.code has any valid attempts - default TRUE
#'
#' It is not clear how the XNAT selection was achieved and the inclusion of invalids should be examined
#'
#' @return data frame complying with input params
#'
#' @export
selectIteration <-
  function(df,
           iterationFunction = min,
           completed = TRUE,
           valid = TRUE) {
    # remove the valid constraint if there is no Validity column in supplied DF
    if (valid & !("Valid" %in% colnames(df))) {
      warning("Validity selection requested but 'Valid' variable not supplied")
      valid <- FALSE
    }
    # Add an index to preserve order after the aggregation
    df$rowIndex <- seq_len(nrow(df))
    
    if (completed) {
      df <- df[df$Completed == 't', ]
    }
    if (valid) {
      # limit to Valid Iterations only if the User.code is ever valid
      df <-
        merge (df,
               setNames(
                 aggregate(Valid ~ User.code,
                           max,
                           data = df),
                 c("User.code", "everValid")
               ),
               by = c("User.code"),
               sort = FALSE)
      df <- df[df$Valid == 't' | df$everValid == 'f', ]
      df <- df[order(df$rowIndex), ]
      df$everValid <- NULL
    }
    
    df <-
      merge (
        df,
        aggregate(Iteration ~ User.code,
                  iterationFunction,
                  data = df),
        by = c("User.code", "Iteration"),
        sort = FALSE
      )
    df <- df[order(df$rowIndex), ]
    df$rowIndex <- NULL
    return(df)
  }

#' Recode select variables in supplied wide df adding "R" suffix to them
#'
#' @param df data frame containing wide form data
#'
#' @param vars list of greppable tokens in the df colnames - each must uniquely identify 1 col
#'
#' @param fun funtion to apply
#'
#' @return modified df
#'
#' @export
recodeVariables <- function(df, varlist, fun) {
  for (i in varlist ){
    if(length(grep(i, names(df))) !=1){stop(paste('Reverse token', i, 'does not uniquely identify one variable in suplied df'))}
    if(exists("customMissingValues")){
      df[!(df[,grep(i, names(df))]  %in% customMissingValues),grep(i, names(df))]<- fun(na.omit(stripCustomMissings(df[,grep(i, names(df))])))
    } else {
      df[,grep(i, names(df))]<- fun(df[,grep(i, names(df))])
    }
    names(df)<-gsub(i, paste(i, 'R', sep=''), names(df))  
  }
  return(df)
}  


#' Coerce numeric vars encoded as character to numeric to facilitate derivations
#'
#' @param df data frame to be converted
#'
#' @return data frame with character encoded numeric variables coerced to numeric
fixNumericVariables <- function(df) {
  df[] <-
    lapply(df, function(x) {
      if (suppressWarnings(!any(is.na(as.numeric(
        as.character(x)
      )) - is.na((x))))) {
        as.numeric(as.character(x))
      }
      else
        x
    })
  return(df)
}


#' Check a df meets minimum specs for processing
#' @param df Data frame to be checked.
#'
#' @param additionalVars extra columns that must be present in the df
#'
#' @param nonRequiredVars standard columns do not need to be present in the df
#'
#' @return boolean
sanityCheck <-
  function(df,
           additionalVars = c(),
           nonRequiredVars = c()) {
    reqVar = c(
      "User.code",
      "Iteration",
      "Language",
      "Completed",
      "Completed.Timestamp",
      "Processed.Timestamp",
      "Block",
      "Trial",
      "Trial.result"
    )
    reqVar <- setdiff(c(reqVar, additionalVars), nonRequiredVars)
    sane <- TRUE
    # Currently just check the required variables are there and the df is not empty
    # TODO allow more fine grained testing
    if (min(reqVar %in% colnames(df)) == 0) {
      sane <- FALSE
      warning("Columns do not meet requirements")
    }
    if (nrow(df) == 0) {
      sane <- FALSE
      warning("Data frame has no rows")
    }
    return(sane)
  }

#' recode defined missign codes back to NA for Row summing purposes
#' @param df Data Frame/Table containg custom missing codes
#'
#' @param customCodes custom missing codes to code to NA
#'
#' @return recoded df/dt
stripCustomMissings <-
  function(df,
           customCodes = c(-999,-888,-777,-666)) {
    for(x in customCodes) {
      df[df==x]<-NA
    }
    return(df)
  }

#' Utility function to calculate row sums first stripping custom missings 
#' and then replacing missing results with a custom missing
#' @param df Data Frame/Table to perform rowSums upon
#'
#' @param missingValue custom missing code to apply to missing results
#'
#' @param maxMissing (0 to 1) return a prorated sum if the number of missings are under this threshold
#'
#' @return recoded df/dt

rowSumsCustomMissing<- function(df, customMissingCodes = c(-999,-888,-777,-666), missingValue = -666, maxMissing = 0) {
  if(maxMissing >1 | maxMissing <0) { stop('Max missing is a proportion ( between 0 and 1 )') }
  na.rm<-ifelse(maxMissing==0, FALSE, TRUE)
  df<-stripCustomMissings(df, customMissingCodes)
  sums<-rowMeans(df, na.rm) * ncol(df)
  nas<-rowSums(is.na(df), na.rm=TRUE)
  sums[nas > maxMissing * ncol(df) ] <- missingValue
  
  return(sums)
}