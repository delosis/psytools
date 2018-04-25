# Derive Psytools CSV files exported from the Delosis server
#
#  Copyright (C) 2017 Delosis
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

#NB These functions expect rowIndex columns to be present in the supplied df
## The DFs supplied are expected to be as they come out of the csv PER TASK other than this I am not removing the extra columns (pause duration etc) which came from merging all tasks together into one DF
## However you do NOT need to have filtered to the correct iteration as iteration will be supplied back in the summary DF
## In short:
##        Load the csv using read.csv
##        compute a row index
##        supply the DF to one of the derive functions.
##
##        repeat for each task separately

## Happy to remove reliance on the row index if you prefer! Some additional flexibility can be worked in

## TODO remove use of reshape - eg BART - slow as molasses

library(data.table)


#' Derive SST data.
#'
#' Applies to the
#' \href{http://dx.doi.org/10.1037/0033-295X.91.3.295}{Stop Signal Task}
#' task implemented in Psytools.
#'
#' Clearly there are some people who are simply not responding for big chunks
#' of this task in c-VEDA. This gives them a good stop hit rate but very bad
#' go_success rate. They will probably need to be excluded. Really this should
#' not be happening in a supervised admin situation?
#'
#' @param df Data frame with SST data, read from CSV file exported from Delosis server.
#'
#' @return Data frame with summaries.
#'
#' @export
deriveSST <- function(df) {
  if (sanityCheck(df, c("rowIndex")) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- subset(df, df$Block != 'SST_Practice')
  df <-
    df[order(df$User.code, df$Iteration, df$rowIndex, df$Trial), ]
  
  # Split the result column
  options(stringsAsFactors = FALSE)
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial.result), ':', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'TrialType'
  names(df)[names(df) == 'X2'] <- 'Stimulus'
  names(df)[names(df) == 'X3'] <- 'StopDelay'
  names(df)[names(df) == 'X4'] <- 'TrialResult'
  names(df)[names(df) == 'X5'] <- 'StopHitRate'
  names(df)[names(df) == 'X6'] <- 'TrialDuration'
  df$StopDelay <- as.numeric(df$StopDelay)
  df$StopHitRate <- as.numeric(df$StopHitRate)
  
  # Summaries - not 100% sure what you need here?
  dfsums <-
    do.call(
      data.frame,
      aggregate(cbind(TrialResult) ~ User.code + Iteration + Language + Completed +
                  Completed.Timestamp + Processed.Timestamp, function(x)
                    c(
                      GO_SUCCESS = length(which(x == "GO_SUCCESS")),
                      GO_TOO_LATE = length(which(x == "GO_TOO_LATE")),
                      GO_WRONG_KEY_RESPONSE = length(which(x == "GO_WRONG_KEY_RESPONSE")),
                      STOP_TOO_EARLY = length(which(x == "STOP_TOO_EARLY")),
                      STOP_FAIL = length(which(x == "STOP_FAIL")),
                      STOP_SUCCESS = length(which(x == "STOP_SUCCESS"))
                    ), data = df)
    )
  dfsums <-
    merge(dfsums,
          do.call(
            data.frame,
            aggregate(cbind(StopDelay, StopHitRate) ~ User.code + Iteration, function(x)
              c(
                mean = mean(x),
                sd = sd(x),
                final = tail(x, 1)
              ),
              na.action = NULL,
              data = subset(df, df$TrialType == 'STOP_VAR'))
          ),
          by = c("User.code", "Iteration"))
  
  return (dfsums)
}


#' Derive MID data.
#'
#' Applies to the
#' \href{https://doi.org/10.1006/nimg.2000.0593}{Monetary Incentive Delay}
#' task implemented in Psytools.
#'
#' @param df Data frame with MID data, read from CSV file exported from Delosis server.
#'
#' @return Data frame with summaries.
#'
#' @export
deriveMID <- function(df) {
  if (sanityCheck(df, c("rowIndex")) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <-
    subset(df, df$Block != 'MID_PRACTICE' &
             df$Block != 'midNRCHECK')
  df <-
    df[order(df$User.code, df$Iteration, df$rowIndex, df$Trial), ]
  
  # Split the result column
  options(stringsAsFactors = FALSE)
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial.result), ':', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'TrialResult'
  names(df)[names(df) == 'X2'] <- 'TargetDuration'
  names(df)[names(df) == 'X3'] <- 'TargetHitRate'
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial), '_', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'TrialNum'
  names(df)[names(df) == 'X4'] <- 'TargetPosition'
  df$TrialType <- paste(df$X2, df$X3)
  df$TrialNum <- as.numeric(df$TrialNum)
  df <- subset(df, select = -c(X2, X3))
  
  # Fix for coding bug with Failure trials (hit rate and target duration
  # not output in early version of task)
  df$TargetDuration[df$TrialResult == "'FAILURE"] <-
    df$TargetDuration[df$TrialResult[-1] == "'FAILURE"]
  df$TargetHitRate[df$TrialResult == "'FAILURE"] <-
    round(
      as.numeric(df$TargetHitRate[df$TrialResult[-1] == "'FAILURE"]) * as.numeric(df$TrialNum[df$TrialResult[-1] ==
                                                                                                "'FAILURE"]) / (as.numeric(df$TrialNum[df$TrialResult[-1] == "'FAILURE"]) +
                                                                                                                  1),
      digits = 2
    )
  df$TrialResult[df$TrialResult == "'FAILURE"] <- "FAILURE"
  
  df$TargetDuration <- as.numeric(df$TargetDuration)
  df$TargetHitRate <- as.numeric(df$TargetHitRate)
  
  # Summaries - not 100% sure what you need here?
  dfsums <-
    do.call(
      data.frame,
      aggregate(cbind(TrialResult) ~ User.code + Iteration + Language + Completed +
                  Completed.Timestamp + Processed.Timestamp, function(x)
                    c(
                      NO_RESPONSE = length(which(x == "NO_RESPONSE")),
                      TOO_LATE = length(which(x == "TOO_LATE")),
                      TOO_EARLY = length(which(x == "TOO_EARLY")),
                      SUCCESS = length(which(x == "SUCCESS")),
                      FAILURE = length(which(x == "FAILURE"))
                    ), data = df)
    )
  dfsums <-
    merge(dfsums,
          do.call(
            data.frame,
            aggregate(cbind(TargetDuration, TargetHitRate) ~ User.code + Iteration, function(x)
              c(
                mean = mean(x),
                sd = sd(x),
                final = tail(x, 1)
              ), data = df)
          ),
          by = c("User.code", "Iteration"))
  
  return (dfsums)
}


#' Derive WCST data.
#'
#' Applies to the Wisconsin Card Sorting Test
#' (part of the \href{https://dx.doi.org/10.1016/j.jneumeth.2013.10.024}{PEBL} battery))
#' implemented in Psytools.
#'
#' @param df Data frame with WCST data, read from CSV file exported from Delosis server.
#'
#' @return Data frame with summaries.
#'
#' @export
deriveWCST <- function(df) {
  if (sanityCheck(df, c("rowIndex")) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- df
  
  # Split the result column
  options(stringsAsFactors = FALSE)
  df <-
    suppressWarnings(cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial.result), '_', fixed = TRUE)
    ))))
  names(df)[names(df) == 'X2'] <- 'SortCategory'
  df$Perseverations[df$X3 == 'PERSEV'] <- 1
  df$Corrects[df$X1 == 'PASS'] <- 1
  df <- subset(df, select = -c(X1, X3))
  df <- df[order(df$User.code, df$Iteration, df$rowIndex), ]
  
  # Flag each switch for summing
  df$Switches <- 0
  df$Switches[df$SortCategory != c(df$SortCategory[-1], NA) &
                df$User.code == c(df$User.code[-1], NA) &
                df$Iteration == c(df$Iteration[-1], NA)] <- 1
  
  # Summaries
  dfsums <-
    do.call(
      data.frame,
      aggregate(
        cbind(Corrects, Switches, Perseverations) ~ User.code + Iteration + Language +
          Completed + Completed.Timestamp + Processed.Timestamp,
        FUN = sum,
        na.rm = TRUE,
        na.action = NULL,
        data = df
      )
    )
  dfsums <-
    merge(dfsums,
          do.call(
            data.frame,
            aggregate(cbind(Response.time..ms.) ~ User.code + Iteration, function(x)
              c(mean = mean(x), sd = sd(x)), data = df)
          ),
          by = c("User.code", "Iteration"))
  
  return (dfsums)
}


#' Derive DS data.
#'
#' Applies to the Digit Span task
#' (part of the \href{https://dx.doi.org/10.1016/j.jneumeth.2013.10.024}{PEBL} battery))
#' implemented in Psytools.
#'
#' @param df Data frame with DS data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveDS <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  #Create a numerical score to sum later
  df$Corrects[df$Trial.result == "PASS"] <- 1
  
  # Summaries
  dfsums <-
    do.call(
      data.frame,
      aggregate(
        cbind(Corrects) ~ User.code + Iteration + Language + Completed + Completed.Timestamp +
          Processed.Timestamp + Block,
        FUN = sum,
        na.rm = TRUE,
        na.action = NULL,
        data = df
      )
    )
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      timevar = "Block"
    )
  
  dfsums$SpanF[dfsums$Corrects.F_2 > 1] <- 2
  dfsums$SpanF[dfsums$Corrects.F_3 > 1] <- 3
  dfsums$SpanF[dfsums$Corrects.F_4 > 1] <- 4
  dfsums$SpanF[dfsums$Corrects.F_5 > 1] <- 5
  dfsums$SpanF[dfsums$Corrects.F_6 > 1] <- 6
  dfsums$SpanF[dfsums$Corrects.F_7 > 1] <- 7
  dfsums$SpanF[dfsums$Corrects.F_8 > 1] <- 8
  dfsums$SpanF[dfsums$Corrects.F_9 > 1] <- 9
  dfsums$SpanF[dfsums$Corrects.F_10 > 1] <- 10
  
  dfsums$SpanB[dfsums$Corrects.B_2 > 1] <- 2
  dfsums$SpanB[dfsums$Corrects.B_3 > 1] <- 3
  dfsums$SpanB[dfsums$Corrects.B_4 > 1] <- 4
  dfsums$SpanB[dfsums$Corrects.B_5 > 1] <- 5
  dfsums$SpanB[dfsums$Corrects.B_6 > 1] <- 6
  dfsums$SpanB[dfsums$Corrects.B_7 > 1] <- 7
  dfsums$SpanB[dfsums$Corrects.B_8 > 1] <- 8
  dfsums$SpanB[dfsums$Corrects.B_9 > 1] <- 9
  dfsums$SpanB[dfsums$Corrects.B_10 > 1] <- 10
  
  return (dfsums)
}


#' Derive CORSI data.
#'
#' Applies to the
#' \href{http://digitool.library.mcgill.ca/R/?func=dbin-jump-full&object_id=93903}{CORSI block-tapping test}
#' (part of the \href{https://dx.doi.org/10.1016/j.jneumeth.2013.10.024}{PEBL} battery))
#' implemented in Psytools.
#'
#' @param df Data frame with CORSI data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveCORSI <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- subset(df, df$Block != 'P2')
  
  #Create a numerical score to sum later
  df$Corrects[df$Trial.result == "PASS"] <- 1
  
  # Summaries
  dfsums <-
    do.call(
      data.frame,
      aggregate(
        cbind(Corrects) ~ User.code + Iteration + Language + Completed + Completed.Timestamp +
          Processed.Timestamp + Block,
        FUN = sum,
        na.rm = TRUE,
        na.action = NULL,
        data = df
      )
    )
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      timevar = "Block"
    )
  
  dfsums$SpanF[dfsums$Corrects.F2 > 0] <- 2
  dfsums$SpanF[dfsums$Corrects.F3 > 0] <- 3
  dfsums$SpanF[dfsums$Corrects.F4 > 0] <- 4
  dfsums$SpanF[dfsums$Corrects.F5 > 0] <- 5
  dfsums$SpanF[dfsums$Corrects.F6 > 0] <- 6
  dfsums$SpanF[dfsums$Corrects.F7 > 0] <- 7
  dfsums$SpanF[dfsums$Corrects.F8 > 0] <- 8
  dfsums$SpanF[dfsums$Corrects.F9 > 0] <- 9
  dfsums$SpanF[dfsums$Corrects.F10 > 0] <- 10
  
  dfsums$SpanB[dfsums$Corrects.B2 > 0] <- 2
  dfsums$SpanB[dfsums$Corrects.B3 > 0] <- 3
  dfsums$SpanB[dfsums$Corrects.B4 > 0] <- 4
  dfsums$SpanB[dfsums$Corrects.B5 > 0] <- 5
  dfsums$SpanB[dfsums$Corrects.B6 > 0] <- 6
  dfsums$SpanB[dfsums$Corrects.B7 > 0] <- 7
  dfsums$SpanB[dfsums$Corrects.B8 > 0] <- 8
  dfsums$SpanB[dfsums$Corrects.B9 > 0] <- 9
  dfsums$SpanB[dfsums$Corrects.B10 > 0] <- 10
  
  return (dfsums)
}


#' Derive TMT data.
#'
#' Applies to the Trail Making Test
#' (part of the \href{https://dx.doi.org/10.1016/j.jneumeth.2013.10.024}{PEBL} battery))
#' implemented in Psytools.
#'
#' Drop the initial practice.
#'
#' @param df Data frame with TMT data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveTMT <- function(df) {
  if (sanityCheck(
    df,
    c(
      "Incorrect.responses",
      "Wild.responses",
      "Pause.duration..ms."
    ),
    c("Trial.result")
  ) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- subset(df,!grepl("Practice", Block, ignore.case = TRUE))
  
  # Simplify the block names
  df$Block <- gsub("TMT_", "", df$Block)
  df$Block <- gsub("_Test[1]?", "", df$Block)
  
  # Summaries
  dfsums <-
    do.call(
      data.frame,
      aggregate(
        cbind(Response.time..ms., Incorrect.responses, Wild.responses) ~ User.code +
          Iteration + Language + Completed + Completed.Timestamp + Processed.Timestamp +
          Block,
        FUN = sum,
        na.rm = TRUE,
        na.action = NULL,
        data = df
      )
    )
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      timevar = "Block"
    )
  
  return (dfsums)
}


#' Derive SOCRATIS data.
#'
#' Applies to the
#' \href{https://doi.org/10.1016/j.ajp.2011.05.014}{Social Cognition Rating Tools in Indian Setting}
#' implemented in Psytools. This is essentially just a questionnaire -
#' other questionnaires could be similarly processed!
#'
#' @param df Data frame with SOCRATIS data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveSOCRATIS <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- subset(df,!grepl("FEEDBACK|js", Block, ignore.case = TRUE))
  
  # Remove unneeded columns and any skip back control markers
  df <-
    subset(
      df,
      df$Response != 'skip_back',
      select = c(
        User.code,
        Iteration,
        Language,
        Completed,
        Completed.Timestamp,
        Processed.Timestamp,
        Trial,
        Trial.result
      )
    )
  
  # Select just the LAST response on each question - note that this means repeating a task will update the results - but it also takes the most recent response if they navigate backwards and then change their mind
  df <-
    df[!duplicated(subset(df, select = c(User.code, Iteration, Trial)), fromLast =
                     TRUE), ]
  
  if (sanityCheck(df, , c("Block")) == FALSE) {
    stop("df does not meet requirements once filtered")
  }
  
  
  # Summaries - currently just showing those calculated in task - let me know if there are any other ones
  df <- subset(df, grepl("INDEX", Trial, ignore.case = TRUE))
  df <-
    reshape(
      df,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      timevar = "Trial"
    )
  names(df) <- gsub("Trial.result.", "", names(df))
  df$SOCRATIS_TOM_1_INDEX <- as.numeric(df$SOCRATIS_TOM_1_INDEX)
  df$SOCRATIS_TOM_2_INDEX <- as.numeric(df$SOCRATIS_TOM_2_INDEX)
  df$SOCRATIS_FAUS_PAS_INDEX <- as.numeric(df$SOCRATIS_FAUS_PAS_INDEX)
  
  return (df)
}


#' Derive BART data.
#'
#' Applies to the
#' \href{https://dx.doi.org/10.1037/1076-898X.8.2.75}{Balloon Analogue Risk Task}
#' implemented in Psytools.
#'
#' @param df Data frame with BART data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveBART <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  # Split the result column
  options(stringsAsFactors = FALSE)
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial.result), '_', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'TrialResult'
  names(df)[names(df) == 'X2'] <- 'PumpsMade'
  df$PumpsMade <- as.numeric(df$PumpsMade)
  
  # Remove the index from the trial column so it can serve as the Colour factor
  df$BalloonColour <- toupper(gsub("[0-9]", "", df$Trial))
  
  # Remove unneeded columns
  df <-
    subset(
      df,
      select = c(
        User.code,
        Iteration,
        Language,
        Completed,
        Completed.Timestamp,
        Processed.Timestamp,
        BalloonColour,
        TrialResult,
        PumpsMade
      )
    )
  
  # Summaries
  dfsums <-
    do.call(
      data.frame,
      aggregate(
        cbind(PumpsMade) ~ User.code + Iteration + Language + Completed + Completed.Timestamp +
          Processed.Timestamp + BalloonColour + TrialResult,
        FUN = sum,
        na.rm = TRUE,
        na.action = NULL,
        data = df
      )
    )
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp",
        "BalloonColour"
      ),
      timevar = "TrialResult"
    )
  dfsums <-
    merge(
      do.call(
        data.frame,
        aggregate(cbind(TrialResult) ~ User.code + Iteration + BalloonColour, function(x)
          c(NumPopped = length(which(x == "POPPED"))), data =
            df)
      ),
      dfsums,
      by = c("User.code", "Iteration", "BalloonColour")
    )
  names(dfsums)[names(dfsums) == 'TrialResult'] <- 'NumPopped'
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      timevar = "BalloonColour"
    )
  
  return (dfsums)
}


#' Derive PALP data.
#'
#' Applies to the
#' \href{https://doi.org/10.1176/appi.ajp.2014.13111499}{PALP task}
#' implemented in Psytools.
#'
#' @param df Data frame with PALP data, read from CSV file exported from Delosis server.
#' If the PALP is administered with separate conditions as separate tasks then they should be merged into a single
#' long df containing all the different conditions
#' NB if merging in this way the individual dfs must be limited to a single Iteration per participant beforehand
#'
#' @return Derived data frame with summaries.
#'
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#'
#' @export
derivePALP <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  # remove the practice and pretreat trials, id_check and ts variables
  df <-
    df[!grepl("practice|pretreat|id_check|ts_|intro", df$Block), ]
  
  # convert the set trial result to a column stim_set
  stimSets <-
    df[df$Block == "set", grepl("User.code|Completed.Timestamp|Iteration|Trial.result",
                                colnames(df))]
  colnames(stimSets)[4] <- "stim_set"
  df <-
    merge(
      df,
      stimSets,
      by = c("User.code", "Completed.Timestamp", "Iteration"),
      sort = FALSE
    )
  #remove the set rows
  df <- df[df$Block != 'set',]
  rm(stimSets)
  
  #Compute an Omission Flag
  df$Omission <- 0
  df$Omission[df$Response != 'space' &
                grepl('INCORRECT', df$Trial.result)] <- 1
  
  #Compute an Commission Flag
  df$Commission <- 0
  df$Commission[df$Response == 'space' &
                  grepl('INCORRECT', df$Trial.result)] <- 1
  
  #Compute a condition variable
  df$Condition <- NA
  df$Condition[grepl('RP', df$Block)] <- "RP"
  df$Condition[grepl('RR', df$Block)] <- "RR"
  df$Condition[grepl('PR', df$Block)] <- "PR"
  df$Condition[grepl('PP', df$Block)] <- "PP"
  
  #Extract the Summary Score into a separate df
  scores <-
    df[grepl("summary", df$Block), grepl("User.code|Condition|Trial.result", colnames(df))]
  scores$Score <- as.numeric(scores$Trial.result)
  scores <-
    scores[, grepl("User.code|Condition|Score", colnames(scores))]
  df <-
    merge(df,
          scores,
          by = c("User.code", "Condition"),
          sort = FALSE)
  #remove the score rows
  df <- df[!grepl("summary", df$Block),]
  colnames(df)[grepl("Response.time..ms.", colnames(df))] <- "RT"
  
  #Compute a block variable
  df$BlockNum <- as.numeric(gsub("R|P|_", "", df$Block))
  
  conditionMeans <-
    dcast(
      setDT(df),
      User.code ~ Condition,
      fun.aggregate = mean,
      na.rm = TRUE,
      value.var = c("Score", "Omission", "Commission", "RT")
    )
  conditionBlockMeans <-
    dcast(
      setDT(df),
      User.code ~ Condition + BlockNum,
      fun.aggregate = mean,
      na.rm = TRUE,
      value.var = c("Omission", "Commission", "RT")
    )
  
  # Set the completed and processed timestamps to the LAST sub task if these were done as separate tasks
  dfsums <-
    merge(
      merge(
        aggregate(
          cbind(Processed.Timestamp, Completed.Timestamp) ~ User.code + Language + Completed,
          FUN = tail,
          n = 1,
          data = df
        ),
        conditionMeans,
        by = c("User.code"),
        all = TRUE
      ),
      conditionBlockMeans,
      by = c("User.code"),
      all = TRUE
    )
  
  #Data.table aggregation introduces NaN's instead of NA if there is nothing to compute - revert these to NA
  dfsums[dfsums == "NaN"] <- NA
  return (setDF(dfsums))
}


#' Derive ERT data.
#'
#' Applies to the
#' \href{https://dx.doi.org/10.2466/PMS.104.2.589-598}{Emotion Recognition Task}
#' implemented in Psytools.
#'
#' @param df Data frame with ERT data, read from CSV file exported from Delosis server.
#' @return Derived data frame with summaries.
#'
#' @export
deriveERT <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  df <- subset(df, df$Block == 'MAIN')
  
  # Split the Trial column
  options(stringsAsFactors = FALSE)
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial), '_', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'TrialEmotion'
  names(df)[names(df) == 'X2'] <- 'TrialEmotionIndex'
  df$TrialEmotionIndex <- as.numeric(df$TrialEmotionIndex)
  
  # Mark the response
  df$Correct <- 0
  df$Correct[df$TrialEmotion == df$Response] <- 1
  
  # Make an RTC and RTI (correct / Incorrect)
  df$RTcorrect[df$Correct == 1] <-
    df$Response.time..ms.[df$Correct == 1]
  df$RTincorrect[df$Correct == 0] <-
    df$Response.time..ms.[df$Correct == 0]
  
  # Remove unneeded columns
  df <-
    subset(
      df,
      select = c(
        User.code,
        Iteration,
        Language,
        Completed,
        Completed.Timestamp,
        Processed.Timestamp,
        Trial,
        Response,
        Response.time..ms.,
        TrialEmotion,
        TrialEmotionIndex,
        Correct,
        RTcorrect,
        RTincorrect
      )
    )
  
  # Summaries
  dfsums <-
    do.call(
      data.frame,
      aggregate(
        cbind(RTcorrect, RTincorrect) ~ User.code + Iteration + Language + Completed +
          Completed.Timestamp + Processed.Timestamp + TrialEmotion,
        FUN = mean,
        na.rm = TRUE,
        na.action = NULL,
        data = df
      )
    )
  dfsums <-
    merge(
      do.call(
        data.frame,
        aggregate(
          cbind(Correct) ~ User.code + Iteration + TrialEmotion,
          FUN = sum,
          na.rm = TRUE,
          na.action = NULL,
          data = df
        )
      ),
      dfsums,
      by = c("User.code", "Iteration", "TrialEmotion")
    )
  dfsums$RTcorrect[dfsums$RTcorrect == 'NaN'] <- NA
  dfsums$RTincorrect[dfsums$RTincorrect == 'NaN'] <- NA
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      timevar = "TrialEmotion"
    )
  
  return (dfsums)
}


#' Derive KIRBY data.
#'
#' Applies to the
#' \href{https://www.ncbi.nlm.nih.gov/pubmed/10100392}{Now-or-later test}
#' (part of the \href{https://dx.doi.org/10.1016/j.jneumeth.2013.10.024}{PEBL} battery))
#' implemented in Psytools. Again just a questionnaire!
#'
#' Note this does not select iterations, though it will only produce the Kests for
#'    Completed datasets
#'
#' @param df Data frame with KIRBY data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries AND raw data.
#' 
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table set
#' 
#' @export
deriveKIRBY <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  # Remove the flags used in cVEDA
  df <-
    subset(
      df,
      !grepl("FEEDBACK|js|KIRBY_PCDELAY", Block, ignore.case = TRUE) &
        df$Trial.result != 'skip_back'
    )
  
  # Some task releases have the block names in lower case...
  df$Block[substr(df$Block, 1, 5) == "kirby"] <-
    toupper(df$Block[substr(df$Block, 1, 5) == "kirby"])
  
  
  #Select out the raw data to go in the same file - including the Number of responses var if available ( some digests )
  dfraw <- df
  if ("Number.of.Responses" %in% colnames(df)) {
    dfraw <- dfraw[substr(dfraw$Block, 1, 5) == "KIRBY",]
    dfraw$Trial <- paste(dfraw$Trial, "Nresponses", sep = "_")
    dfraw$Trial.result <- dfraw$Number.of.Responses
    dfraw <- rbind(df, dfraw)
  }
  dfraw <- rotateQuestionnaire(dfraw)
  
  # Remove anything that is not a Kirby block ( id check , ts in the case of imagen)
  df <- df[substr(df$Block, 1, 5) == "KIRBY",]
  
  # Select just the LAST response on each question - note that this means repeating a task will update the results - but it also takes the most recent response if they navigate backwards and then change their mind
  df <-
    df[!duplicated(subset(df, select = c(User.code, Iteration, Trial)), fromLast =
                     TRUE), ]
  if (sanityCheck(
    df,
    c(
      "User.code",
      "Iteration",
      "Language",
      "Completed",
      "Completed.Timestamp",
      "Processed.Timestamp",
      "Trial",
      "Block",
      "Trial.result"
    )
  ) == FALSE) {
    stop("df does not meet requirements once filtered")
  }
  
  #Convert to DT to speed up the iterative processing
  df<-setDT(df)
  
  # Add the computed Kind values
  df$Kind[df$Block == 'KIRBY01'] <- 0.000158277936055715
  df$Kind[df$Block == 'KIRBY02'] <- 0.00596125186289121
  df$Kind[df$Block == 'KIRBY03'] <- 0.00595829195630586
  df$Kind[df$Block == 'KIRBY04'] <- 0.248847926267281
  df$Kind[df$Block == 'KIRBY05'] <- 0.0413533834586466
  df$Kind[df$Block == 'KIRBY06'] <- 0.000398936170212766
  df$Kind[df$Block == 'KIRBY07'] <- 0.102564102564103
  df$Kind[df$Block == 'KIRBY08'] <- 0.1
  df$Kind[df$Block == 'KIRBY09'] <- 0.000158277936055713
  df$Kind[df$Block == 'KIRBY10'] <- 0.00604838709677419
  df$Kind[df$Block == 'KIRBY11'] <- 0.246753246753247
  df$Kind[df$Block == 'KIRBY12'] <- 0.00100338642919854
  df$Kind[df$Block == 'KIRBY13'] <- 0.00595829195630586
  df$Kind[df$Block == 'KIRBY14'] <- 0.0405643738977072
  df$Kind[df$Block == 'KIRBY15'] <- 0.00254817646121994
  df$Kind[df$Block == 'KIRBY16'] <- 0.00252235725750975
  df$Kind[df$Block == 'KIRBY17'] <- 0.000398089171974522
  df$Kind[df$Block == 'KIRBY18'] <- 0.0158045977011494
  df$Kind[df$Block == 'KIRBY19'] <- 0.101731601731602
  df$Kind[df$Block == 'KIRBY20'] <- 0.000399042298483639
  df$Kind[df$Block == 'KIRBY21'] <- 0.0156862745098039
  df$Kind[df$Block == 'KIRBY22'] <- 0.0025
  df$Kind[df$Block == 'KIRBY23'] <- 0.0414634146341463
  df$Kind[df$Block == 'KIRBY24'] <- 0.001001001001001
  df$Kind[df$Block == 'KIRBY25'] <- 0.0160493827160494
  df$Kind[df$Block == 'KIRBY26'] <- 0.00100267379679144
  df$Kind[df$Block == 'KIRBY27'] <- 0.25
  
  # Add the LDR scale
  df$LDRscale[df$Block == 'KIRBY01'] <- 2
  df$LDRscale[df$Block == 'KIRBY02'] <- 3
  df$LDRscale[df$Block == 'KIRBY03'] <- 1
  df$LDRscale[df$Block == 'KIRBY04'] <- 3
  df$LDRscale[df$Block == 'KIRBY05'] <- 1
  df$LDRscale[df$Block == 'KIRBY06'] <- 2
  df$LDRscale[df$Block == 'KIRBY07'] <- 1
  df$LDRscale[df$Block == 'KIRBY08'] <- 2
  df$LDRscale[df$Block == 'KIRBY09'] <- 3
  df$LDRscale[df$Block == 'KIRBY10'] <- 2
  df$LDRscale[df$Block == 'KIRBY11'] <- 1
  df$LDRscale[df$Block == 'KIRBY12'] <- 3
  df$LDRscale[df$Block == 'KIRBY13'] <- 1
  df$LDRscale[df$Block == 'KIRBY14'] <- 2
  df$LDRscale[df$Block == 'KIRBY15'] <- 3
  df$LDRscale[df$Block == 'KIRBY16'] <- 2
  df$LDRscale[df$Block == 'KIRBY17'] <- 3
  df$LDRscale[df$Block == 'KIRBY18'] <- 1
  df$LDRscale[df$Block == 'KIRBY19'] <- 3
  df$LDRscale[df$Block == 'KIRBY20'] <- 1
  df$LDRscale[df$Block == 'KIRBY21'] <- 2
  df$LDRscale[df$Block == 'KIRBY22'] <- 1
  df$LDRscale[df$Block == 'KIRBY23'] <- 3
  df$LDRscale[df$Block == 'KIRBY24'] <- 2
  df$LDRscale[df$Block == 'KIRBY25'] <- 3
  df$LDRscale[df$Block == 'KIRBY26'] <- 1
  df$LDRscale[df$Block == 'KIRBY27'] <- 2
  
  # This analysis only works for completed attempts - remove any early terminations
  df <-
    df[order(df$User.code, df$Iteration, df$LDRscale, df$Kind), ]
  df <- subset(df, df$Completed == "t")
  
  ####RECODE refuse to 0 - the calculations will fail otherwise - this is a slight biasing move but hard to see how else to avoid removing them completely?
  df$Trial.result[df$Trial.result == 'refuse'] <- 0
  
  ## First work out Kest by LDRscale
  df[, TrialOrderIdx := seq(.N), by = c("User.code", "Iteration", "LDRscale")]
  # Sum of higher and equal k choices which are 1 (LDR)
  for (i in 1:nrow(df)) {
    set(df,
        i,
        "Consistency",
        sum(as.numeric(df$Trial.result[i:(i + 9 - df$TrialOrderIdx[i])])) +
          # Plus the number of lower k choices = 0
          ifelse(
            df$TrialOrderIdx[i] == 1,
            0,
            (df$TrialOrderIdx[i] - 1) -
              sum(as.numeric(df$Trial.result[(i - (df$TrialOrderIdx[i] - 1)):(i - 1)]))
          ))
  }
  
  # Add a max consistency field
  df <-
    merge(
      df,
      aggregate(Consistency ~ User.code + Iteration + LDRscale, function(x)
        c(mean = max(as.numeric(x))), data = df),
      by = c("User.code", "Iteration", "LDRscale"),
      suffixes = c("", ".max"),
      all.x = TRUE
    )
  df$Kest <- NA
  # Calculate the Kest field for each max consistency - based on the geo mean of the max and preceding
  for (i in 1:nrow(df)) {
    if (df$Consistency[i] == df$Consistency.max[i]) {
      df$Kest[i] <- exp(mean(log(c(
        df$Kind[i], df$Kind[i - 1]
      ))))
    }
  }
  # Finally make a geomean of all the max consistencies geomeans as their final outcome
  dfsums <-
    do.call(
      data.frame,
      aggregate(cbind(Kest) ~ User.code + Iteration + LDRscale, function(x)
        c(geomean = exp(mean(log(
          x
        )))), data = df)
    )
  dfsums <-
    reshape(
      dfsums,
      direction = "wide",
      idvar = c("User.code", "Iteration"),
      timevar = "LDRscale"
    )
  
  ## Next overall
  df <- df[order(df$User.code, df$Iteration, df$Kind), ]
  df <- subset(df, select = -c(TrialOrderIdx, Consistency, Consistency.max))
  #Create a trial order index
  df[, TrialOrderIdx := seq(.N), by = c("User.code", "Iteration")]
  system.time(# Sum of higher and equal k choices which are 1 (LDR)
    for (i in 1:nrow(df)) {
      set(df,
          i,
          "Consistency",
          sum(as.numeric(df$Trial.result[i:(i + 27 - df$TrialOrderIdx[i])])) +
            # Plus the number of lower k choices = 0
            ifelse(
              df$TrialOrderIdx[i] == 1,
              0,
              (df$TrialOrderIdx[i] - 1) -
                sum(as.numeric(df$Trial.result[(i - (df$TrialOrderIdx[i] - 1)):(i - 1)]))
            ))
    })
  
  # Add a max consistency field
  df <-
    merge(
      df,
      aggregate(Consistency ~ User.code + Iteration, function(x)
        c(mean = max(as.numeric(x))), data = df),
      by = c("User.code", "Iteration"),
      suffixes = c("", ".max"),
      all.x = TRUE
    )
  df$Kest <- NA
  # Calculate the Kest field for each max consistency - based on the geo mean of the max and preceding
  for (i in 1:nrow(df)) {
    if (df$Consistency[i] == df$Consistency.max[i]) {
      df$Kest[i] <- exp(mean(log(c(
        df$Kind[i], df$Kind[i - 1]
      ))))
    }
  }
  
  # Finally make a geomean of all the max consistencies geomeans as their final outcome - Merge it into the Kest by LDRscale from earlier
  dfsums <-
    merge(do.call(
      data.frame,
      aggregate(cbind(Kest) ~ User.code + Iteration + Language + Completed +
                  Completed.Timestamp + Processed.Timestamp, function(x)
                    c(geomean = exp(mean(log(
                      x
                    )))), data = df)
    ),
    dfsums,
    by = c("User.code", "Iteration"))
  
  #Merge the raw data into the summary
  dfsums <-
    merge(
      dfraw,
      dfsums[, grepl("User.code|Iteration|Kest", colnames(dfsums))],
      by = c("User.code", "Iteration"),
      sort = FALSE,
      all.x = TRUE
    )
  return (dfsums)
}

#' Rotate simple questionnaires from long to wide format.
#'
#' Requires \code{User.code}, \code{Iteration}, \code{Trial} and \code{Trial.result}
#' columns in input data frame.
#' Removes repeated occurrences of \code{Trial.result} caused by skipping back.
#' Should work for any questionnaire to rotate into a wide format, but may want some additional honing!
#'
#' Preserves the Valid column if it is included in the supplied DF
#'
#' @param BlockAsMeasureVar 
#' @param df Data frame with simple questionnaire, read from CSV file exported from Delosis server.
#'
#' @return Rotated data frame.
#'
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#'
#' @export
rotateQuestionnaire <-
  function(df,
           BlockAsMeasureVar = FALSE,
           idVar = c(
             "User.code",
             "Iteration",
             "Language",
             "Completed",
             "Completed.Timestamp",
             "Processed.Timestamp"
           )) {
  nonRequiredVars<-setdiff(c(
    "User.code",
    "Iteration",
    "Language",
    "Completed",
    "Completed.Timestamp",
    "Processed.Timestamp"
  ), idVar)
  if (sanityCheck(df, , nonRequiredVars) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  measureVar = c("Trial")
  if (BlockAsMeasureVar) {
    measureVar = c("Block", measureVar)
  }
  
  #Keep in the Valid column if it exists
  if ("Valid" %in% colnames(df)) {
    idVar = c(idVar, "Valid")
  }
  
  df <- setDT(df)
  
  # Remove the results generated when displaying the feedback from instruments such as the Mini
  df <-
    df[!grepl("FEEDBACK", df$Block, ignore.case = T) &
         df$Trial.result != 'skip_back',]
  
  # Select only the last response for each question in cases of skipping back and revising.
  # only the first 2 idvars are needed
  df <-
    df[!duplicated(subset(df, select = c(head(idVar,2), measureVar)), fromLast =
                     T),]
  
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements once filtered")
  }
  
  df <-
    dcast(subset(df,
                 select = c(idVar, measureVar, "Trial.result")),
          as.formula(paste(
            paste(idVar, collapse = "+"),
            "~" ,
            paste(measureVar, collapse = "+"),
            sep = " "
          )),
          value.var = "Trial.result")
  
  
  return (setDF(fixNumericVariables(df)))
}


#' Rotate simple questionnaires from long to wide format.
#' Preserving Block as well as trial for the output
#'
#' Requires \code{User.code}, \code{Iteration}, \code{Block},\code{Trial} and \code{Trial.result}
#' columns in input data frame.
#' Removes repeated occurrences of \code{Trial.result} caused by skipping back.
#' Should work for any questionnaire to rotate into a wide format, but may want some additional honing!
#'
#' @param df Data frame with simple questionnaire, read from CSV file exported from Delosis server.
#' 
#' @return Rotated data frame.
#' 
#' @export
rotateQuestionnairePreserveBlock <- function(df) {
  return (rotateQuestionnaire(df, TRUE))
}


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
      df <- df[df$Completed == 't',]
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
      df <- df[df$Valid == 't' | df$everValid == 'f',]
      df <- df[order(df$rowIndex),]
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
    df <- df[order(df$rowIndex),]
    df$rowIndex <- NULL
    return(df)
  }


#' Generate summary for CTS questionnaires (from all stages)
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' The FU / FU2 and FU3 files use a slightly different structure - this is homogenised here
#'
#' @param df data frame containing long form CTS data
#'
#' @return wide form of Reliability data with summary vars
#'
#' @export
deriveImgnCTS <- function(df) {
  df$Trial[as.numeric(df$Trial) < 10] <-
    paste(0, df$Trial[as.numeric(df$Trial) < 10], sep = "")
  dfScore <- df
  df$Trial <- paste("Response", df$Trial, sep = ".")
  dfScore$Trial <- paste("Score", dfScore$Trial, sep = ".")
  df$Trial.result <- as.numeric(substr(df$Trial.result, 1, 1))
  dfScore$Response <- as.numeric(substr(dfScore$Trial.result, 1, 1))
  dfScore$Trial.result <-
    as.numeric(substr(dfScore$Trial.result, 3, 3))
  #Apply score recodes specified in SPSS syntax (see Strauss 96)
  dfScore$Trial.result[dfScore$Response == 3] <- 4
  dfScore$Trial.result[dfScore$Response == 4] <- 8
  dfScore$Trial.result[dfScore$Response == 5] <- 15
  dfScore$Trial.result[dfScore$Response == 6] <- 25
  df <- rbind(df, dfScore)
  df <- rotateQuestionnaire(df)
  
  # Compute summary Variables from SPSS syntax
  df$ccamyp <-
    rowMeans(cbind(
      df$Score.08,
      df$Score.10,
      df$Score.18,
      df$Score.46,
      df$Score.54
    ))
  df$ccamys <-
    rowMeans(cbind(
      df$Score.07,
      df$Score.09,
      df$Score.17,
      df$Score.45,
      df$Score.53
    ))
  df$ccasyp <-
    rowMeans(
      cbind(
        df$Score.22,
        df$Score.28,
        df$Score.34,
        df$Score.38,
        df$Score.44,
        df$Score.62,
        df$Score.74
      )
    )
  df$ccasys <-
    rowMeans(
      cbind(
        df$Score.21,
        df$Score.27,
        df$Score.33,
        df$Score.37,
        df$Score.43,
        df$Score.61,
        df$Score.73
      )
    )
  df$ccimyp <- rowMeans(cbind(df$Score.12, df$Score.72))
  df$ccimys <- rowMeans(cbind(df$Score.11, df$Score.71))
  df$ccisyp <-
    rowMeans(cbind(df$Score.24, df$Score.32, df$Score.42, df$Score.56))
  df$ccisys <-
    rowMeans(cbind(df$Score.23, df$Score.31, df$Score.41, df$Score.55))
  df$ccncyp <-
    rowMeans(cbind(df$Score.04, df$Score.60, df$Score.78))
  df$ccncys <-
    rowMeans(cbind(df$Score.03, df$Score.59, df$Score.77))
  df$ccneyp <-
    rowMeans(cbind(df$Score.02, df$Score.14, df$Score.40))
  df$ccneys <-
    rowMeans(cbind(df$Score.01, df$Score.13, df$Score.39))
  df$ccpmyp <-
    rowMeans(cbind(df$Score.06, df$Score.36, df$Score.50, df$Score.68))
  df$ccpmys <-
    rowMeans(cbind(df$Score.05, df$Score.35, df$Score.49, df$Score.67))
  df$ccpsyp <-
    rowMeans(cbind(df$Score.26, df$Score.30, df$Score.66, df$Score.70))
  df$ccpsys <-
    rowMeans(cbind(df$Score.25, df$Score.29, df$Score.65, df$Score.69))
  df$ccsmyp <-
    rowMeans(cbind(df$Score.16, df$Score.52, df$Score.64))
  df$ccsmys <-
    rowMeans(cbind(df$Score.15, df$Score.51, df$Score.63))
  df$ccssyp <-
    rowMeans(cbind(df$Score.20, df$Score.48, df$Score.58, df$Score.76))
  df$ccssys <-
    rowMeans(cbind(df$Score.19, df$Score.47, df$Score.67, df$Score.75))
  df$cts_assault <-
    rowMeans(cbind(df$ccamyp, df$ccamys, df$ccasyp , df$ccasys))
  df$cts_injury <-
    rowMeans(cbind(df$ccimyp, df$ccimys, df$ccisyp , df$ccisys))
  df$cts_negotiation <-
    rowMeans(cbind(df$ccncyp, df$ccncys, df$ccneyp , df$ccneys))
  df$cts_psychological_aggression <-
    rowMeans(cbind(df$ccpmyp, df$ccpmys, df$ccpsyp , df$ccpsys))
  df$cts_sexual_coercion <-
    rowMeans(cbind(df$ccsmyp, df$ccsmys, df$ccssyp , df$ccssys))
  
  return(df)
}


#' Generate summary for Reliability questionnaires (from all stages)
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' The FU / FU2 and FU3 files use a slightly different structure - this is homogenised here
#'
#' @param df data frame containing long form Reliability data
#'
#' @return wide form of Reliability data with summary vars
#'
#' @export
deriveImgnReliability <- function(df) {
  # homogenise the variable naming/coding for FU FU2 FU3
  df$Trial <- gsub("comment", "_specify", df$Trial)
  df$Trial <- gsub("result", "", df$Trial)
  df$Trial.result[df$Trial.result == "missing"] <- "M"
  df$Trial.result[df$Trial.result == "good"] <- "G"
  df$Trial.result[df$Trial.result == "doubtful"] <- "D"
  df$Trial.result[df$Trial.result == "bad"] <- "B"
  df$Trial.result[df$Trial.result == "not_applicable"] <- "NA"
  #apply the standard rotation
  df <- rotateQuestionnaire(df)
  return(df)
}


#' Generate summary for ADRS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form ADRS data
#'
#' @return wide form of ADRS data with summary vars
#'
#' @export
deriveImgnADRS <- function(df) {
  #Recode
  df$Trial.result[df$Trial.result == "2" &
                    substr(df$Trial, 1, 4) == "adrs"] <- 0
  #Rotate
  df <- rotateQuestionnaire(df)
  #Summary
  df$adrs_sum <-
    rowSums(df[, grepl("adrs", colnames(df))])
  return(df)
}


#' Generate summary for CIS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form CIS data
#'
#' @return wide form of CIS data with summary vars
#'
#' @export
deriveImgnCIS <- function(df) {
  #Recode
  df$Trial.result[df$Trial.result == "3" &
                    substr(df$Trial, 1, 4) == "item"] <- 4
  df$Trial.result[df$Trial.result == "2" &
                    substr(df$Trial, 1, 4) == "item"] <- 3
  #Rotate
  df <- rotateQuestionnaire(df)
  #Summary
  df$csi_sum <-
    rowSums(df[, grepl("item", colnames(df))])
  return(df)
}


#' Generate summary for IRI questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' QUERY - in supplied docs Perspective sum contains IRI25 twice?
#' I have followed this (i.e. counting 25 twice) please check
#'
#' @param df data frame containing long form IRI data
#'
#' @return wide form of IRI data with summary vars
#'
#' @export
deriveImgnIRI <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  #Summary
  df$IRI_perspective <-
    rowSums(df[, grepl("I03|I08|I11|I15|I21|I25", colnames(df))], na.rm =
              TRUE) + df$IRI25
  df$IRI_fantasy <-
    rowSums(df[, grepl("I01|I05|I07|I12|I16|I23|I26", colnames(df))], na.rm =
              TRUE)
  df$IRI_empathic <-
    rowSums(df[, grepl("I02|I04|I09|I14|I18|I20|I22", colnames(df))], na.rm =
              TRUE)
  df$IRI_personal <-
    rowSums(df[, grepl("I06|I10|I13|I17|I19|I24|I27", colnames(df))], na.rm =
              TRUE)
  return(df)
}


#' Generate summary for PDS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form PDS data
#'
#' @return wide form of PDS data with summary vars
#'
#' @export
deriveImgnPDS <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  #Convert Imperial to Metric if Administered ( not at BL )
  if ("mass_kg" %in% colnames(df)) {
    df$mass_kg[is.na(df$mass_kg) &
                 !is.na(df$mass_lb) &
                 df$mass_lb > 0] <-
      0.453592 * df$mass_lb[is.na(df$mass_kg) &
                              !is.na(df$mass_lb) & df$mass_lb > 0]
    df$height_cm[is.na(df$height_cm) &
                   !is.na(df$height_inches) &
                   df$height_inches > 0] <-
      2.54 * df$height_inches[is.na(df$height_cm) &
                                !is.na(df$height_inches) &
                                df$height_inches > 0]
  }
  #Summary cannot be calculated at FU2 - these items not administered
  if ("a8" %in% colnames(df)) {
    df$PDS_mean <-
      rowMeans(df[, grepl("a8|a9|a10|a11|a12a_f|a12_m", colnames(df))], na.rm = TRUE)
  }
  return(df)
}

#' Generate summary for AUDIT questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' Note that in the case of no alcohol consumption this returns 0 for the summaries
#'   The original SPSS did not do this but it seems appropriate
#'
#' @param df data frame containing long form AUDIT data
#'
#' @return wide form of AUDIT data with summary vars
#'
#' @export
deriveImgnAUDIT <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summary Vars
  df$audit_freq <-
    rowSums(cbind(df$audit1, df$audit2, df$audit3), na.rm = TRUE)
  df$audit_symp <-
    rowSums(cbind(df$audit4, df$audit6, df$audit8), na.rm = TRUE)
  df$audit_prob <-
    rowSums(cbind(df$audit5, df$audit7, df$audit9, df$audit10), na.rm = TRUE)
  df$audit_total <-
    rowSums(cbind(df$audit_freq, df$audit_symp, df$audit_prob), na.rm = TRUE)
  return(df)
}


#' Generate summary for MAST questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' Note that in the case of no alcohol consumption this returns 0 for the summaries
#'   The original SPSS did not do this but it seems appropriate
#'
#' @param df data frame containing long form MAST data
#'
#' @return wide form of MAST data with summary vars
#'
#' @export
deriveImgnMAST <- function(df) {
  #The Parent version does not have "mast" in the trial names
  df$Trial[grepl("^[1-9]", df$Trial)] <-
    paste("mast", df$Trial[grepl("^[1-9]", df$Trial)], sep = "")
  
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Recodes - Some items are weighted according to the SPSS script
  df$mast8 <- 5 * df$mast8
  df$mast20 <- 5 * df$mast20
  df$mast21a <- 5 * df$mast21a
  df$mast1 <- 2 * df$mast1
  df$mast2 <- 2 * df$mast2
  df$mast4 <- 2 * df$mast4
  df$mast6 <- 2 * df$mast6
  df$mast7 <- 2 * df$mast7
  df$mast10 <- 2 * df$mast10
  df$mast11 <- 2 * df$mast11
  df$mast12 <- 2 * df$mast12
  df$mast13 <- 2 * df$mast13
  df$mast14 <- 2 * df$mast14
  df$mast15 <- 2 * df$mast15
  df$mast17 <- 2 * df$mast17
  df$mast24 <- 2 * df$mast24
  df$mast25 <- 2 * df$mast25
  
  #Summaries
  df$mast_total <-
    rowSums(df[, grepl("mast", colnames(df))], na.rm = TRUE)
  df$mast_total[df$mast1 == -2 &
                  !is.na(df$mast1)] <-
    df$mast_total[df$mast1 == -2 & !is.na(df$mast1)] + 2
  df$mast_dsm <-
    rowSums(df[, grepl("26|27|28|29|30|31", colnames(df))], na.rm = TRUE)
  df$mast_sum <- df$mast_total - df$mast_dsm
  return(df)
}


#' Generate summary for NEO FFI questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' Note that in the case of no alcohol consumption this returns 0 for the summaries
#'   The original SPSS did not do this but it seems appropriate
#'
#' @param df data frame containing long form NEO FFI data
#'
#' @return wide form of NEO FFI data with summary vars
#'
#' @export
deriveImgnNEO <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summaries
  df$neur_mean <-
    rowMeans(df[, grepl("1$|6$", colnames(df))])
  df$extr_mean <-
    rowMeans(df[, grepl("2$|7$", colnames(df))])
  df$open_mean <-
    rowMeans(df[, grepl("3$|8$", colnames(df))])
  df$agre_mean <-
    rowMeans(df[, grepl("4$|9$", colnames(df))])
  df$cons_mean <-
    rowMeans(df[, grepl("5$|0$", colnames(df))])
  return(df)
}


#' Generate summary for SURPS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' Note that in the case of no alcohol consumption this returns 0 for the summaries
#'   The original SPSS did not do this but it seems appropriate
#'
#' @param df data frame containing long form SURPS data
#'
#' @return wide form of SURPS data with summary vars
#'
#' @export
deriveImgnSURPS <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summaries
  df$as_mean <-
    rowMeans(df[, grepl("s8|s10|s14|s18|s21", colnames(df))])
  df$h_mean <-
    rowMeans(df[, grepl("s1$|s4|s7|s13|s17|s20|s23", colnames(df))])
  df$imp_mean <-
    rowMeans(df[, grepl("s2$|s5|s11|s15|s22", colnames(df))])
  df$ss_mean <-
    rowMeans(df[, grepl("s3$|s6|s9|s12|s26|s19", colnames(df))])
  return(df)
}


#' Generate summary for TCI questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' Note that in the case of no alcohol consumption this returns 0 for the summaries
#'   The original SPSS did not do this but it seems appropriate
#'
#' @param df data frame containing long form TCI data
#'
#' @return wide form of TCI data with summary vars
#'
#' @export
deriveImgnTCI <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summaries
  df$tci_excit <-
    rowSums(df[, grepl("001|063|053|104|122|145|156|165|176|205", colnames(df))])
  df$tci_imp <-
    rowSums(df[, grepl("010|047|071|102|123|179|193|210|239", colnames(df))])
  df$tci_extra <-
    rowSums(df[, grepl("014|024|059|105|139|155|172|215|222", colnames(df))])
  df$tci_diso <-
    rowSums(df[, grepl("044|051|077|109|135|159|170", colnames(df))])
  df$tci_novseek <-
    rowSums(df[, grepl("tci_", colnames(df))])
  return(df)
}


#' Generate summary for ESPAD questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form ESPAD data
#'
#' @return wide form of ESPAD data with summary vars
#'
#' @export
deriveImgnESPAD <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summary Vars
  df$ftnd_sum <-
    rowSums(df[, grepl("ftnd", colnames(df))] , na.rm = TRUE)
  for (drug in c(
    "amphet",
    "anabolic",
    "cannabis",
    "coke",
    "crack",
    "ghb",
    "glue",
    "heroin",
    "ketamine",
    "lsd",
    "mdma",
    "mushrooms",
    "narcotic",
    "relevin",
    "tranq"
  )) {
    df[[paste("dast", drug, "sum", sep = "_")]] <-
      rowSums(df[, grepl(paste("dast[0-9]+", drug, sep = ""), colnames(df))], na.rm =
                TRUE)
    #add 1 if they have anything as dast1 is not administered but has been answered if anything endorsed
    df[[paste("dast", drug, "sum", sep = "_")]] <-
      ifelse(df[[paste("dast", drug, "sum", sep = "_")]] > 0, df[[paste("dast", drug, "sum", sep =
                                                                          "_")]] + 1, df[[paste("dast", drug, "sum", sep = "_")]])
  }
  
  return(df)
}


#' Generate summary for GENETIC History questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form GEN data
#'
#' @return wide form of GEN data with summary vars
#' 
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' 
#' @export
deriveImgnGEN <- function(df) {
  #long format processing
  df$Trial[df$Block == "Psych_History_Knowledge"] <- "Psych_History"
  #remove the relation on which they exit
  df <- df[df$Response != 'exit_button', ]
  #Convert the relation / sure / disorder rows into variables
  df <-
    merge(
      df,
      setNames(df[df$Trial == 'relation', grepl("User.code|Iteration|Block|Trial.result", colnames(df))], c(
        "User.code", "Iteration", "Block", "Relation"
      )),
      by = c("User.code", "Iteration", "Block"),
      all.x = TRUE,
      sort = FALSE
    )
  df <-
    merge(
      df,
      setNames(df[df$Trial == 'sure', grepl("User.code|Iteration|Block|Trial.result", colnames(df))], c(
        "User.code", "Iteration", "Block", "Sure"
      )),
      by = c("User.code", "Iteration", "Block"),
      all.x = TRUE,
      sort = FALSE
    )
  df <-
    merge(
      df,
      setNames(df[df$Trial == 'disorder', grepl("User.code|Iteration|Block|Trial.result", colnames(df))], c(
        "User.code", "Iteration", "Block", "Disorder"
      )),
      by = c("User.code", "Iteration", "Block"),
      all.x = TRUE,
      sort = FALSE
    )
  # Rotate out the non-relation questions into a wide df
  dfEth <-
    rotateQuestionnaire(df[substr(df$Block, 1, 8) != "Relation",])
  #remove undefined relations ( there will always be one at the end )
  #Also remove the disgnosis and sure rows - not needed now we have the variables
  df <-
    df[df$Relation != "" & df$Trial != "sure" & df$Trial != "disorder", ]
  df <- df[!is.na(df$User.code), ]
  
  #Order by relation within iteration
  df <- df[order(df$User.code, df$Iteration, df$Relation), ]
  ## Iterate through and subscript the relation with an index so multiple diagnoses per relation can be supported
  df$RelationOrderIdx <- 1
  for (i in 1:nrow(df)) {
    if (i > 1) {
      if (df[i, ]$User.code == df[i - 1, ]$User.code &
          df[i, ]$Iteration == df[i - 1, ]$Iteration &
          df[i, ]$Relation == df[i - 1, ]$Relation) {
        df$RelationOrderIdx[i] <- df$RelationOrderIdx[i - 1] + 1
      } else {
        df$RelationOrderIdx[i] <- 1
      }
    }
  }
  
  df$Relation <- paste(df$Relation, df$RelationOrderIdx, sep = "_")
  
  df <-
    dcast(
      setDT(df),
      User.code + Iteration ~ Relation,
      na.rm = TRUE,
      value.var = c("Sure", "Disorder")
    )
  setDF(df)
  
  df <- merge(dfEth,
              df,
              by = c("User.code", "Iteration"),
              all.x = TRUE)
  rm(dfEth)
  return(df)
}


#' Derive IDENT (Morph) data.
#'
#' Applies to the
#' Morphed Emotions Task implemented in Psytools for the Imagen Study .
#' Will be applicable for other Morph type paradigms
#'
#' Note this function processes all Iterations provided
#' The calling function should first select the iteration required
#'
#' This is NOT calculating the thresholds as the original SPSS script did
#' This could be added in but Frauke did not request them.
#'
#' @param df Data frame with IDENT data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#' 
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' 
#' @export
deriveImgnIDENT <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  #Rotate out the id_check and ts variables if they exist to a separate df to merge in later
  withTSID <- FALSE
  if (nrow(df[substr(df$Block, 1, 3) == 'ts_' |
              substr(df$Block, 1, 3) == 'id_', ]) > 0) {
    withTSID <- TRUE
    dfTsID <-
      rotateQuestionnaire(df[substr(df$Block, 1, 3) == 'ts_' |
                               substr(df$Block, 1, 3) == 'id_', ])
    df <-
      df[!(substr(df$Block, 1, 3) == 'ts_' |
             substr(df$Block, 1, 3) == 'id_'), ]
  }
  
  #Select just the Main block
  df <- df[df$Block == 'IDENT_MAIN', ]
  
  # Split the Trial and response columns
  options(stringsAsFactors = FALSE)
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial), '_', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'FaceID'
  names(df)[names(df) == 'X2'] <- 'ContA'
  names(df)[names(df) == 'X3'] <- 'ContB'
  names(df)[names(df) == 'X4'] <- 'Morph'
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Response), '_', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'Response_side'
  names(df)[names(df) == 'X2'] <- 'Response_emotion'
  df$Morph <- 10 * as.numeric(df$Morph)
  
  #Create a response var indicating if they chose the 0% or 100% end of the continuum
  df$P[df$Response_emotion == df$ContA] <- 0
  df$P[df$Response_emotion == df$ContB] <- 100
  df$P <- as.numeric(df$P)
  names(df)[names(df) == 'Response.time..ms.'] <- 'RT'
  
  dfsums <-
    dcast(
      setDT(df),
      User.code + Iteration ~ ContA + ContB + Morph,
      fun.aggregate = mean,
      na.rm = TRUE,
      value.var = c("P", "RT")
    )
  setDF(dfsums)
  if (withTSID) {
    dfsums <-
      merge(
        dfTsID,
        dfsums,
        by = c("User.code", "Iteration"),
        sort = FALSE,
        all.x = TRUE
      )
  }
  return (dfsums)
}


#' Derive DOT_PROBE (Morph) data.
#'
#' Applies to the
#' Dot Probe implemented in Psytools for the Imagen Study .
#' Will be applicable for other dot_probe type paradigms
#'
#' Note this function processes all Iterations provided
#' The calling function should first select the iteration required
#'
#' @param df Data frame with DOT_PROBE data, read from CSV file exported from Delosis server.
#'
#' @return Derived data frame with summaries.
#' 
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#'
#' @export
deriveImgnDOTPROBE <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  #Rotate out the id_check and ts variables if they exist to a separate df to merge in later
  withTSID <- FALSE
  if (nrow(df[substr(df$Block, 1, 3) == 'ts_' |
              substr(df$Block, 1, 3) == 'id_', ]) > 0) {
    withTSID <- TRUE
    dfTsID <-
      rotateQuestionnaire(df[substr(df$Block, 1, 3) == 'ts_' |
                               substr(df$Block, 1, 3) == 'id_', ])
    df <-
      df[!(substr(df$Block, 1, 3) == 'ts_' |
             substr(df$Block, 1, 3) == 'id_'), ]
  }
  
  #Select just the Main block
  df <- df[df$Block == 'DOT_PROBE_MAIN', ]
  
  # Split the Trial column
  options(stringsAsFactors = FALSE)
  df <-
    cbind(df, data.frame(do.call(
      'rbind', strsplit(as.character(df$Trial), '_', fixed = TRUE)
    )))
  names(df)[names(df) == 'X1'] <- 'Emotion'
  names(df)[names(df) == 'X2'] <- 'Congruence'
  names(df)[names(df) == 'X3'] <- 'dotSide'
  names(df)[names(df) == 'X4'] <- 'faceID'
  
  #Create a response var indicating if they chose the 0% or 100% end of the continuum
  df$SCORE[df$Trial.result == "PASS"] <- 1
  df$SCORE[df$Trial.result == "FAIL"] <- 0
  df$SCORE <- as.numeric(df$SCORE)
  names(df)[names(df) == 'Response.time..ms.'] <- 'RT'
  
  dfsums <-
    dcast(
      setDT(df),
      User.code + Iteration ~ Emotion + Congruence,
      fun.aggregate = c(mean, sum),
      na.rm = TRUE,
      value.var = c("SCORE", "RT")
    )
  setDF(dfsums)
  
  # Remove the RT_sum and SCORE_mean variables
  dfsums <- dfsums[, !grepl("RT_sum|SCORE_mean", colnames(dfsums))]
  if (withTSID) {
    dfsums <-
      merge(
        dfTsID,
        dfsums,
        by = c("User.code", "Iteration"),
        sort = FALSE,
        all.x = TRUE
      )
  }
  return (dfsums)
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
