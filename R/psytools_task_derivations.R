# Derive Psytools CSV files exported from the Delosis server
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

#NB These functions expect rowIndex columns to be present in the supplied df
## The DFs supplied are expected to be as they come out of the csv PER TASK other than this I am not removing the extra columns (pause duration etc) which came from merging all tasks together into one DF
## However you do NOT need to have filtered to the correct iteration as iteration will be supplied back in the summary DF
## In short:
##        Load the csv using read.csv
##        compute a row index
##        supply the DF to one of the derive functions.
##
##        repeat for each task separately

## rowIndex can be suplied indicating original row ordering -
## this will be created if needed but it is important the df is supplied in the original row order in that case


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
  if(!"rowIndex" %in% names(df)){
    df$rowIndex <- seq_len(nrow(df))
  }
  if (sanityCheck(df, c("rowIndex")) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  # Save task version info
  settings<-rotateQuestionnaire(df[df$Block=='Settings',])
  settings<-settings[,grepl('User.code|Iteration|TaskVersion', names(settings))]
  
  
  df <- subset(df, df$Block == 'SST_Main')
  df<-merge(df, settings, by=c('User.code','Iteration'), all=T)
  df$TaskVersion[is.na(df$TaskVersion)]<-'IMAGEN'
  
  
  df <-
    df[order(df$User.code, df$Iteration, df$rowIndex, df$Trial),]
  
  
  
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
                  Completed.Timestamp + Processed.Timestamp + TaskVersion, function(x)
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
  if(!"rowIndex" %in% names(df)){
    df$rowIndex <- seq_len(nrow(df))
  }
  if (sanityCheck(df, c("rowIndex")) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <-
    subset(df, df$Block != 'MID_PRACTICE' &
             df$Block != 'midNRCHECK')
  df <-
    df[order(df$User.code, df$Iteration, df$rowIndex, df$Trial),]
  
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
  if(!"rowIndex" %in% names(df)){
    df$rowIndex <- seq_len(nrow(df))
  }
  
  if (sanityCheck(df, c("rowIndex")) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
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
  df <- df[order(df$User.code, df$Iteration, df$rowIndex),]
  
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
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveDS <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- setDT(df)
  
  #Create a numerical score to sum later
  df[Trial.result == 'PASS', Corrects := 1]
  df[Trial.result != 'PASS', Corrects := 0]
  
  # Calculate the Number correct within each block
  # Fill with 'defaultUnadministeredValue' if it has been defined to show which blocks were not attempted due to task termination
  df <- dcast(
    df,
    User.code + Iteration + Language +
      Completed +
      Completed.Timestamp +
      Processed.Timestamp ~ paste('Corrects.', Block, sep = '') ,
    fun = sum,
    value.var = 'Corrects',
    sep = '.',
    fill = ifelse(exists('defaultUnadministeredValue'), defaultUnadministeredValue, NA)
  )
  
  # If they fail the easiest block then they get -777 for the span
  
  df[Corrects.F_2 <= 1, SpanF := -777]
  df[Corrects.F_2 > 1, SpanF := 2]
  df[Corrects.F_3 > 1, SpanF := 3]
  df[Corrects.F_4 > 1, SpanF := 4]
  df[Corrects.F_5 > 1, SpanF := 5]
  df[Corrects.F_6 > 1, SpanF := 6]
  df[Corrects.F_7 > 1, SpanF := 7]
  df[Corrects.F_8 > 1, SpanF := 8]
  df[Corrects.F_9 > 1, SpanF := 9]
  df[Corrects.F_10 > 1, SpanF := 10]
  df[Corrects.B_2 <= 1, SpanF := -777]
  df[Corrects.B_2 > 1, SpanB := 2]
  df[Corrects.B_3 > 1, SpanB := 3]
  df[Corrects.B_4 > 1, SpanB := 4]
  df[Corrects.B_5 > 1, SpanB := 5]
  df[Corrects.B_6 > 1, SpanB := 6]
  df[Corrects.B_7 > 1, SpanB := 7]
  df[Corrects.B_8 > 1, SpanB := 8]
  df[Corrects.B_9 > 1, SpanB := 9]
  df[Corrects.B_10 > 1, SpanB := 9]
  return (setDF(df))
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
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveCORSI <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  df <- setDT(df)
  df <- df[Block != 'P2', ]
  
  #Create a numerical score to sum later
  df[Trial.result == 'PASS', Corrects := 1]
  df[Trial.result != 'PASS', Corrects := 0]
  
  # Calculate the Number correct within each block
  # Fill with 'defaultUnadministeredValue' to show which blocks were not attempted due to task termination
  df <- dcast(
    df,
    User.code + Iteration + Language +
      Completed +
      Completed.Timestamp +
      Processed.Timestamp ~ paste('Corrects.', Block, sep = '') ,
    fun = sum,
    value.var = 'Corrects',
    sep = '.',
    fill = ifelse(exists('defaultUnadministeredValue'), defaultUnadministeredValue, NA)
  )
  
  # If they fail the easiest block then they get -777 for the span
  
  df[Corrects.F2 == 0, SpanF := -777]
  df[Corrects.F2 > 0, SpanF := 2]
  df[Corrects.F3 > 0, SpanF := 3]
  df[Corrects.F4 > 0, SpanF := 4]
  df[Corrects.F5 > 0, SpanF := 5]
  df[Corrects.F6 > 0, SpanF := 6]
  df[Corrects.F7 > 0, SpanF := 7]
  df[Corrects.F8 > 0, SpanF := 8]
  df[Corrects.F9 > 0, SpanF := 9]
  df[Corrects.B2 == 0, SpanB := -777]
  df[Corrects.B2 > 0, SpanB := 2]
  df[Corrects.B3 > 0, SpanB := 3]
  df[Corrects.B4 > 0, SpanB := 4]
  df[Corrects.B5 > 0, SpanB := 5]
  df[Corrects.B6 > 0, SpanB := 6]
  df[Corrects.B7 > 0, SpanB := 7]
  df[Corrects.B8 > 0, SpanB := 8]
  df[Corrects.B9 > 0, SpanB := 9]
  return (setDF(df))
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
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table setnames
#' @importFrom data.table set
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
  
  df <- subset(df, !grepl("Practice", Block, ignore.case = TRUE))
  
  # Simplify the block names
  df$Block <- gsub("TMT_", "", df$Block)
  df$Block <- gsub("_Test[1]?", "", df$Block)
  
  setDT(df)
  
  # Remove error records - they are counted in the "Pass" record for the trial
  df<-df[Trial.result!='error',]
  
  # Produce a table of Ppts who timeout and on what block, needed to code to unadministered
  timeouts<-df[Trial.result=='TIMEOUT', c('User.code', 'Iteration', 'Block')]
  
  setnames(df, 'Response.time..ms.', 'RT')
  setnames(timeouts, 'Block', 'timeoutBlock')
  
  # Summaries
  dfsumsDT <- dcast(
    df,
    User.code +
      Iteration + Language + Completed + Completed.Timestamp + Processed.Timestamp ~ Block,
    fun = sum,
    na.rm = TRUE,
    value.var = c('RT' ,'Incorrect.responses', 'Wild.responses'),
    fill = NA,
    sep='.'
  )
  
  dfsumsDT<-merge(dfsumsDT, timeouts, by=c('User.code', 'Iteration'), all=TRUE)
  
  # Code blocks on which there was a timeout to be defaultUnadministeredValue it wasnt finished and so the data is without meaning
  missingCode<-ifelse(exists('defaultUnadministeredValue'), defaultUnadministeredValue, NA)
  set(dfsumsDT, which(dfsumsDT$timeoutBlock=='Flea'), names(dfsumsDT)[grepl('Flea|Letters', names(dfsumsDT))], missingCode )
  set(dfsumsDT, which(dfsumsDT$timeoutBlock=='Letters'), names(dfsumsDT)[grepl('Letters', names(dfsumsDT))], missingCode )
  set(dfsumsDT, which(dfsumsDT$timeoutBlock=='NumbersLetters'), names(dfsumsDT)[grepl('NumbersLetters', names(dfsumsDT))], missingCode )
  dfsumsDT$timeoutBlock<-NULL
  
  return (setDF(dfsumsDT))
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
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#'
#' @return Derived data frame with summaries.
#'
#' @export
deriveSOCRATIS <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  df <- subset(df, !grepl("FEEDBACK|js", Block, ignore.case = TRUE))
  
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
                     TRUE),]
  
  if (sanityCheck(df, , c("Block")) == FALSE) {
    stop("df does not meet requirements once filtered")
  }
  
  
  # Summaries - currently just showing those calculated in task - let me know if there are any other ones
  #df <- subset(df, grepl("INDEX", Trial, ignore.case = TRUE))
  
  setDT(df)
  
  df <- dcast(
    df,
    User.code + Iteration + Language +
      Completed +
      Completed.Timestamp +
      Processed.Timestamp ~ Trial ,
    value.var = 'Trial.result',
    sep = '.',
    fill = ifelse(exists('defaultUnadministeredValue'), defaultUnadministeredValue, NA)
  )
  df$SOCRATIS_TOM_1_INDEX <- as.numeric(df$SOCRATIS_TOM_1_INDEX)
  df$SOCRATIS_TOM_2_INDEX <- as.numeric(df$SOCRATIS_TOM_2_INDEX)
  df$SOCRATIS_FAUS_PAS_INDEX <-
    as.numeric(df$SOCRATIS_FAUS_PAS_INDEX)
  
  return (setDF(df))
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
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
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
  
  setDT(df)
  
  # Remove the index from the trial column so it can serve as the Colour factor
  df[, BalloonColour := toupper(gsub("[0-9]", "", Trial))]
  df[TrialResult == 'POPPED', NumPopped := 1]
  
  df <- dcast(
    df,
    User.code + Iteration + Language +
      Completed +
      Completed.Timestamp +
      Processed.Timestamp ~ TrialResult + BalloonColour ,
    fun = sum,
    value.var = c('PumpsMade', 'NumPopped'),
    sep = '.'
  )
  #Remove the numpopped collect columns - they are obviously NA
  df[, grep("NumPopped.COLLECT", names(df)) := NULL]
  
  #Rename the Numpopped.POPPED to remove the popped factor to maintain compatibility with previous version of the function
  names(df) <- gsub('NumPopped.POPPED', 'NumPopped', names(df))
  
  return (setDF(df))
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
    df[!grepl("practice|pretreat|id_check|ts_|intro", df$Block),]
  
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
  df <- df[df$Block != 'set', ]
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
  df <- df[!grepl("summary", df$Block), ]
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
#' @importFrom data.table dcast
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table set
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
  
  # Summaries - sum of correct and means of RTI / RTC by  emotion
  dfsums <-
    merge(
      dcast(
        setDT(df),
        User.code + Iteration + Language + Completed +
          Completed.Timestamp + Processed.Timestamp ~ paste('Correct', TrialEmotion, sep = '.'),
        fun = sum,
        na.rm = TRUE,
        value.var = 'Correct',
        sep = '.'
      )
      ,
      dcast(
        setDT(df),
        User.code + Iteration ~ TrialEmotion,
        fun = mean,
        na.rm = TRUE,
        value.var = c('RTcorrect', 'RTincorrect'),
        sep = '.'
      ),
      by = c('User.code', 'Iteration')
    )
  
  # Recode the NaNs in the case of their being no correct or incorrect RTs to take a mean of - > NA
  for(col in grep('RT',names(dfsums))) set(dfsums, i=which(dfsums[[col]]=='NaN'), j=col, value=NA)
  
  
  return (setDF(dfsums))
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
#' @importFrom data.table setnames
#'
#' @export
deriveKIRBY <- function(df) {
  if (sanityCheck(df) == FALSE) {
    stop("df does not meet requirements as passed")
  }
  
  # Remove the flags used in cVEDA
  df <-
    subset(
      df,!grepl("FEEDBACK|js|KIRBY_PCDELAY", Block, ignore.case = TRUE) &
        df$Trial.result != 'skip_back'
    )
  
  # Some task releases have the block names in lower case...
  df$Block[substr(df$Block, 1, 5) == "kirby"] <-
    toupper(df$Block[substr(df$Block, 1, 5) == "kirby"])
  
  
  #Select out the raw data to go in the same file - including the Number of responses var if available ( some digests )
  dfraw <- df
  if ("Number.of.Responses" %in% colnames(df)) {
    dfraw <- dfraw[substr(dfraw$Block, 1, 5) == "KIRBY", ]
    dfraw$Trial <- paste(dfraw$Trial, "Nresponses", sep = "_")
    dfraw$Trial.result <- dfraw$Number.of.Responses
    dfraw <- rbind(df, dfraw)
  }
  dfraw <- rotateQuestionnaire(dfraw)
  
  # Remove anything that is not a Kirby block ( id check , ts in the case of imagen)
  df <- df[substr(df$Block, 1, 5) == "KIRBY", ]
  
  # Select just the LAST response on each question - note that this means repeating a task will update the results - but it also takes the most recent response if they navigate backwards and then change their mind
  df <-
    df[!duplicated(subset(df, select = c(User.code, Iteration, Trial)), fromLast =
                     TRUE),]
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
  df <- setDT(df)
  
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
  setorder(df, User.code, Iteration, LDRscale, Kind)
  df <- subset(df, df$Completed == "t")
  
  ####RECODE refuse to 0 - the calculations will fail otherwise - this is a slight biasing move but hard to see how else to avoid removing them completely?
  df$Trial.result[df$Trial.result == 'refuse'] <- 0
  
  ## First work out Kest by LDRscale
  df[, TrialOrderIdx := seq(.N), by = c("User.code", "Iteration", "LDRscale")]
  # Sum of higher and equal k choices which are 1 (LDR)
  # TODO refine this with a Non Iterative method - it's not outrageously slow as is though
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
  df[, Consistency.max := max(Consistency), by = c("User.code", "Iteration", "LDRscale")]
  
  # Calculate the Kest field for each max consistency - based on the geo mean of the max and preceding (within LDR scale!)
  df[TrialOrderIdx > 1, Kest := exp(rowMeans(log(cbind(
    Kind, shift(Kind, 1, type = "lag")
  ))))]
  df[TrialOrderIdx == 1, Kest := exp(log(Kind))]
  #remove all KEST values where consistency is not max (much quicker to calculate them and then remove)
  df[Consistency != Consistency.max, Kest := NA]
  
  # Finally make a geomean of all the max consistencies geomeans as their final outcome
  dfsums <-
    dcast(df, User.code + Iteration  ~ paste('Kest', LDRscale, sep = '.'), function(x)
      c(geomean = exp(mean(log(
        x
      ), na.rm = TRUE))), value.var = 'Kest')
  
  
  ## Next overall
  # Reset order for overall and remove previous calculation columns
  setorder(df, User.code, Iteration, Kind)
  df[, c("TrialOrderIdx", "Consistency", "Consistency.max", "Kest") := NULL]
  
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
  df[, Consistency.max := max(Consistency), by = c("User.code", "Iteration")]
  
  # Calculate the Kest field for each max consistency - based on the geo mean of the max and preceding (within LDR scale!)
  df[TrialOrderIdx > 1, Kest := exp(rowMeans(log(cbind(
    Kind, shift(Kind, 1, type = "lag")
  ))))]
  df[TrialOrderIdx == 1, Kest := exp(log(Kind))]
  #remove all KEST values where consistency is not max (much quicker to calculate them and then remove)
  df[Consistency != Consistency.max, Kest := NA]
  
  # Finally make a geomean of all the max consistencies geomeans as their final outcome
  dfsums <-
    merge(setnames(
      dcast(df, User.code + Iteration ~ ., function(x)
        c(geomean = exp(mean(log(
          x
        ), na.rm = TRUE))), value.var = 'Kest'),
      '.',
      'Kest'
    ),
    dfsums,
    by = c("User.code", "Iteration"))
  
  #Merge the raw data into the summary
  dfsums <-
    merge(
      dfraw,
      dfsums,
      by = c("User.code", "Iteration"),
      sort = FALSE,
      all.x = TRUE
    )
  return (setDF(dfsums))
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
           skippedValue=NA,
           idVar = c(
             "User.code",
             "Iteration",
             "Language",
             "Completed",
             "Completed.Timestamp",
             "Processed.Timestamp"
           )) {
    nonRequiredVars <- setdiff(
      c(
        "User.code",
        "Iteration",
        "Language",
        "Completed",
        "Completed.Timestamp",
        "Processed.Timestamp"
      ),
      idVar
    )
    
    # replace the passed skippedValue with the session parameter if it exists
    if(exists('defaultUnadministeredValue')) {
      skippedValue <- defaultUnadministeredValue
    }
    
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
           df$Trial.result != 'skip_back', ]
    
    # Select only the last response for each question in cases of skipping back and revising.
    # only the first 2 idvars are needed
    df <-
      df[!duplicated(subset(df, select = c(head(idVar, 2), measureVar)), fromLast =
                       T), ]
    
    if (sanityCheck(df) == FALSE) {
      stop("df does not meet requirements once filtered")
    }
    
    # Rotate and code any skipped or unadministered variables (Not present in the long form) with the specified code
    df <-
      dcast(subset(df,
                   select = c(idVar, measureVar, "Trial.result")),
            as.formula(paste(
              paste(idVar, collapse = "+"),
              "~" ,
              paste(measureVar, collapse = "+"),
              sep = " "
            )),
            fill = skippedValue,
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
rotateQuestionnairePreserveBlock <- function(df, skippedValue=NA) {
  return (rotateQuestionnaire(df, TRUE, skippedValue))
}




#' Generate summary for Alabama Parenting Questionnaire, Child or Parent
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form APQ data
#'
#' @return wide form of APQ data with summary vars
#'
#' @export
deriveAPQ <- function(df) {
  # Remove some stray max volume trials attempts from an early version of the task if they exist
  df <- df[df$Trial != 'MaxVolume', ]
  
  #Rotate
  df <- rotateQuestionnaire(df)
  #Summary
  
  if ('APQ_C_01' %in% names(df)) {
    df$m_involvement <-
      rowSums(stripCustomMissings(df[, grepl("01$|04$|07$|09$|11$|14$|15$|20$|23$|26$", colnames(df))]), na.rm = FALSE)
    df$m_involvement[is.na(df$m_involvement)]<- -666
    
    df$p_involvement <-
      rowSums(stripCustomMissings(df[, grepl("01A$|04A$|07A$|09A$|11A$|14A$|15A$|20A$|23$|26A$",
                                             colnames(df))]), na.rm = FALSE)
    df$p_involvement[is.na(df$p_involvement)]<- -666
  } else {
    df$involvement <-
      rowSums(stripCustomMissings(df[, grepl("01$|04$|07$|09$|11$|14$|15$|20$|23$|26$", colnames(df))]), na.rm = FALSE)
    df$involvement[is.na(df$involvement)]<- -666
  }
  
  df$pos_parenting <-
    rowSums(stripCustomMissings(df[, grepl("02$|05$|13$|16$|18$|27$", colnames(df))]), na.rm = FALSE)
  df$pos_parenting[is.na(df$pos_parenting)]<- -666
  
  df$pr_monitoring <-
    rowSums(stripCustomMissings(df[, grepl("06$|10$|17$|19$|21$|24$|28$|29$|30$|32$", colnames(df))]), na.rm = FALSE)
  df$pr_monitoring[is.na(df$pr_monitoring)]<- -666
  
  df$inc_discipline <-
    rowSums(stripCustomMissings(df[, grepl("03$|08$|12$|22$|25$|31$", colnames(df))]), na.rm = FALSE)
  df$inc_discipline[is.na(df$inc_discipline)]<- -666
  
  df$corp_punishment <-
    rowSums(stripCustomMissings(df[, grepl("33$|35$|39$", colnames(df))]), na.rm = FALSE)
  df$corp_punishment[is.na(df$corp_punishment)]<- -666
  
  df$other_discipline <-
    rowSums(stripCustomMissings(df[, grepl("34$|36$|37$|39$|40$|41$|42$", colnames(df))]), na.rm = FALSE)
  df$other_discipline[is.na(df$other_discipline)]<- -666
  
  return(df)
}

#' Generate summary for Alabama Parenting Questionnaire, Child or Parent
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form APQ data
#'
#' @return wide form of APQ data with summary vars
#'
#' @export
derivePBI <- function(df) {
  # Remove some stray max volume trials attempts from an early version of the task if they exist
  df <- df[df$Trial != 'MaxVolume', ]
  
  #Rotate
  df <- rotateQuestionnaire(df)
  #Summary
  
  calcCareOverprotection<-function (df, stub) {
    care<-rowSums(
        stripCustomMissings(
          df[, grepl('01|02|04|05|06|11|12|14|16|17|18|24', names(df))]
        ), na.rm=FALSE
      )
    overprotection<-rowSums(
        stripCustomMissings(
          df[, grepl('03|07|08|10|13|15|19|20|21|22|23|25', names(df))]
        ), na.rm=FALSE
      )
    df <- data.frame(cbind(care,overprotection))
    df[is.na(df)]<- -666
    names(df)<-c(paste(stub, 'Care', sep = '_'),
                 paste(stub, 'OverProtection', sep = '_'))
    return(df)
  }
  df<-cbind(df,
            calcCareOverprotection(df[,grepl('_F_', names(df))], 'F'),
            calcCareOverprotection(df[,grepl('_M_', names(df))], 'M')
  )

  
  return(df)
}

#' Generate summary for BIG5
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' NB items 2, 6, 8, 9, 12, 18, 21, 23, 24, 27, 31, 34, 35, 37, 41, 43 are reverse coded in place - they are reversed in the returned df with an R suffix
#'
#' @param df data frame containing long form BIG5 data
#'
#' @return wide form of BIG5 data with summary vars 
#'
#' @export
deriveBIG5 <- function(df) {
  # Remove some stray max volume trials attempts from an early version of the task if they exist
  df <- df[df$Trial != 'MaxVolume', ]
  
  #Rotate
  df <- rotateQuestionnaire(df)
  
  # Reverse coding of selected variables
  reverseVariables <-
    c('02', '06', '08', '09', 12, 18, 21, 23, 24, 27, 31, 34, 35, 37, 41, 43)

  df<-recodeVariables(df, reverseVariables, fun= function(x) {6-x})

  #Summary
  df$extraversion <-
    rowSums(stripCustomMissings(df[, grepl("01|06R|11|16|21R|26|31R|36", colnames(df))]), na.rm = FALSE)
  df$extraversion[ is.na(df$extraversion)]<- -666
  
  df$agreeableness <-
    rowSums(stripCustomMissings(df[, grepl("02R|07|12R|17|22|27R|32|37R|42", colnames(df))]), na.rm = FALSE)
  df$agreeableness[ is.na(df$agreeableness)]<- -666
  
  df$conscientiousness <-
    rowSums(stripCustomMissings(df[, grepl("03|08R|13|18R|23R|28|33|38|43R", colnames(df))]), na.rm = FALSE)
  df$conscientiousness[ is.na(df$conscientiousness)]<- -666
  
  df$neuroticism <-
    rowSums(stripCustomMissings(df[, grepl("04|09R|14|19|24R|29|34R|39", colnames(df))]), na.rm = FALSE)
  df$neuroticism[ is.na(df$neuroticism)]<- -666
  
  df$openness <-
    rowSums(stripCustomMissings(df[, grepl("05|10|15|20|25|30|35R|40|41R|44", colnames(df))]), na.rm = FALSE)
  df$openness[ is.na(df$openness)]<- -666
  
    
  return(df)
}

#' Generate summary for AAQ
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' NB items A_x and GCP_x are reverse coded in place - they are reversed in the returned df with an R suffix
#'
#' @param df data frame containing long form AAQ data
#'
#' @return wide form of AAQ data with summary vars 
#'
#' @export
deriveAAQ <- function(df) {
  # Remove some stray max volume trials attempts from an early version of the task if they exist
  df <- df[df$Trial != 'MaxVolume', ]
  
  #Rotate
  df <- rotateQuestionnaire(df)
  
  # Reverse coding of selected variables
  reverseVariables <-
    c('A_1', 'A_2', 'A_3', 'GCP_1','GCP_2','GCP_3' )
  
  df<-recodeVariables(df, reverseVariables, fun= function(x) {4-x})
  
  #Summary
  df$ADsum <-
    rowSums(stripCustomMissings(df[, grepl("_AD_", colnames(df))]), na.rm = FALSE)
  df$Asum <-
    rowSums(stripCustomMissings(df[, grepl("_A_", colnames(df))]), na.rm = FALSE)
  df$GCPsum <-
    rowSums(stripCustomMissings(df[, grepl("_GCP_", colnames(df))]), na.rm = FALSE)
  
  return(df)
}



#' Generate summary for IFVCS
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form IFVCS data
#'
#' @return wide form of IFVCS data with summary vars 
#'
#' @export
deriveIFVCS <- function(df) {
  # Remove some stray max volume trials attempts from an early version of the task if they exist
  df <- df[df$Trial != 'MaxVolume', ]
  
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summary
  df$ControlScore <-
    rowSums(stripCustomMissings(df[, grepl("CONTROL", colnames(df))]), na.rm = TRUE)
  df$PhysicalAbuse <-
    rowSums(stripCustomMissings(df[, grepl("PHYSICAL", colnames(df))]), na.rm = TRUE)
  df$PsychologicalAbuse <-
    rowSums(stripCustomMissings(df[, grepl("PSYCH", colnames(df))]), na.rm = TRUE)
  df$SexualAbuse <-
    rowSums(stripCustomMissings(df[, grepl("SEXUAL", colnames(df))]), na.rm = TRUE)
  
  return(df)
}


#' Generate summary for SDQ questionnaire
#' 
#' Will correct coding if using a 1 based scheme and reverses variables in place adding an R suffix
#'
#' Works for Parent / Self / Teacher forms generating impact scores if data is supplied
#' 
#' NB This does not select attempts - this should be done by the calling function
#'
#' @param df data frame containing long form SDQ data
#'
#' @return wide form of SDQ data with summary vars
#'
#' @export
deriveSDQ <- function(df) {
  #Convert 1 based to 0 based coding if needed
  selector01to25<-paste(
    c(
      paste("0", seq(1, 9), sep = ""),
      seq(10, 25)),
    collapse = "|")
  
  if(max(stripCustomMissings(df$Trial.result[grepl(selector01to25,df$Trial)]), na.rm=TRUE) ==3 & 
     min(stripCustomMissings(df$Trial.result[grepl(selector01to25,df$Trial)]), na.rm=TRUE) ==1) {
      message('recoding 1 based to 0 based response coding')
      df$Trial.result[grepl(selector01to25,df$Trial) & df$Trial.result ==1]<-0
      df$Trial.result[grepl(selector01to25,df$Trial) & df$Trial.result ==2]<-1
      df$Trial.result[grepl(selector01to25,df$Trial) & df$Trial.result ==3]<-2
  }
  
  if(max(stripCustomMissings(df$Trial.result[grepl(selector01to25,df$Trial)]), na.rm=TRUE) !=2 | 
     min(stripCustomMissings(df$Trial.result[grepl(selector01to25,df$Trial)]), na.rm=TRUE) !=0) {
      warning('Cannot confirm that coding of items 1 to 25 is 0 based - please check')
  }
  
  # rotate
  df <- rotateQuestionnaire(df)
  
  # reverse code
  reverseVariables <- c('07', '11', '14', '21', '25')
  df<-recodeVariables(df, reverseVariables, fun= function(x) {2-x})
  
  # Summary
  df$SDQ_EMO_PROB<-rowSumsCustomMissing(
      df[,grepl('03|08|13|16|24', names(df))],
      maxMissing=0.4
    )

  df$SDQ_COND_PROB<-rowSumsCustomMissing(
    df[,grepl('05|07R|12|18|22', names(df))],
    maxMissing=0.4
  )
  
  df$SDQ_HYPER<-rowSumsCustomMissing(
    df[,grepl('02|10|15|21R|25R', names(df))],
    maxMissing=0.4
  )
  
  df$SDQ_PEER_PROB<-rowSumsCustomMissing(
    df[,grepl('06|11R|14R|19|23', names(df))],
    maxMissing=0.4
  )
  
  df$SDQ_PROSOCIAL<-rowSumsCustomMissing(
    df[,grepl('01|04|09|17|20', names(df))],
    maxMissing=0.4
  )
  
  df$SDQ_EXTERNALIZING<-rowSumsCustomMissing(df[,grepl('COND_PROB|HYPER', names(df))])
  df$SDQ_INTERNALIZING<-rowSumsCustomMissing(df[,grepl('PEER_PROB|EMO_PROB', names(df))])
  df$SDQ_TOTAL_DIFFICULTIES<-rowSumsCustomMissing(df[,grepl('EXTERNALIZING|INTERNALIZING', names(df))])
  return(df)
} 

#' Generate summary for SCQ questionnaire
#' 
#' Items 5, 9, 13, 17 are reverse coded and returned with R suffix
#' 
#' NB This does not select attempts - this should be done by the calling function
#'
#' @param df data frame containing long form SCQ data
#'
#' @return wide form of SCQ data with summary vars
#'
#' @export
deriveSCQ <- function(df) {

  # rotate
  df <- rotateQuestionnaire(df)
  
  # reverse code
  reverseVariables <- c('05', '09', '13', '17')
  df<-recodeVariables(df, reverseVariables, fun= function(x) {5-x})
  
  # Summary
  df$SCQ_SAFETY_ORDER<-rowSumsCustomMissing(
        df[,grepl('01|05R|09R|13R|17R', names(df))]
      )
  
  df$SCQ_SAFETY_ORDER2<-rowSumsCustomMissing(
    df[,grepl('01|05R|09R|13R|17R', names(df))]
    )

  df$SCQ_SUPPORT_ACCEPTANCE<-rowSumsCustomMissing(
        df[,grepl('02|06|10|14|18', names(df))]
    )

  df$SCQ_EQUITY_FAIRNESS<-rowSumsCustomMissing(
        df[,grepl('03|07|11|15|19', names(df))]
    )

  df$SCQ_ENCOURAGING_AUTONOMY<-rowSumsCustomMissing(
        df[,grepl('04|08|12|16|20|21', names(df))]
      )

  return(df)
} 

  
#' Generate summary for ASSIST Questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form ASSIST data
#'
#' @return wide form of ASSIST data with summary vars
#'
#' @export
deriveASSIST <- function(df) {
  
  # Remove some stray max volume trials attempts from an early version of the task if they exist
  df <- df[df$Trial != 'MaxVolume', ]
  
  # Recode the elevated codes used for cVEDA due to translation issue
  # Safe in general function as these codes only exist for cVEDA
  df$Trial.result[(df$Block == 'ASSIST_3_a' | df$Block == 'ASSIST_5_a') &
                    df$Trial.result == 10] <- 2
  df$Trial.result[(df$Block == 'ASSIST_3_a' | df$Block == 'ASSIST_5_a') &
                    df$Trial.result == 30] <- 3
  df$Trial.result[(df$Block == 'ASSIST_3_a' | df$Block == 'ASSIST_5_a') &
                    df$Trial.result == 40] <- 4
  df$Trial.result[(df$Block == 'ASSIST_3_a' | df$Block == 'ASSIST_5_a') &
                    df$Trial.result == 10] <- 6
  df$Trial.result[df$Block == 'ASSIST_6_a' &
                    df$Trial.result == 10] <- 4
  df$Trial.result[df$Block == 'ASSIST_6_a' &
                    df$Trial.result == 20] <- 5
  df$Trial.result[df$Block == 'ASSIST_6_a' &
                    df$Trial.result == 30] <- 6
  df$Trial.result[df$Block == 'ASSIST_6_a' &
                    df$Trial.result == 40] <- 7
  df$Trial.result[df$Block == 'ASSIST_7_a' &
                    df$Trial.result == 10] <- 5
  df$Trial.result[df$Block == 'ASSIST_7_a' &
                    df$Trial.result == 20] <- 6
  df$Trial.result[df$Block == 'ASSIST_7_a' &
                    df$Trial.result == 30] <- 7
  df$Trial.result[df$Block == 'ASSIST_7_a' &
                    df$Trial.result == 40] <- 8
  
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summary
  df$prescription <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_a$", colnames(df))]), na.rm = TRUE)
  
  df$tobacco <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_b$", colnames(df))]), na.rm = TRUE)
  
  df$alcohol <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_c$", colnames(df))]), na.rm = TRUE)
  
  df$cannabis <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_d$", colnames(df))]), na.rm = TRUE)
  
  df$inhalants <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_e$", colnames(df))]), na.rm = TRUE)
  
  df$sleeping_pills <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_f$", colnames(df))]), na.rm = TRUE)
  
  df$opioids <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_g$", colnames(df))]), na.rm = TRUE)
  
  df$ats <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_h$", colnames(df))]), na.rm = TRUE)
  
  df$cocaine <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_i$", colnames(df))]), na.rm = TRUE)
  
  df$hallucinogens <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_j$", colnames(df))]), na.rm = TRUE)
  
  df$other <-
    rowSums(stripCustomMissings(df[, grepl("[356789]_k$", colnames(df))]), na.rm = TRUE)
  
  return(df)
}


