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
#' @param df Data frame or table with simple questionnaire, read from CSV file exported from Delosis server.
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
    if(length(grep(i, names(df))) > 1){stop(paste('Reverse token', i, 'does not uniquely identify one variable in suplied df'))}
    if(exists("customMissingValues")){
      df[!(df[,grep(i, names(df))]  %in% customMissingValues),grep(i, names(df))]<- fun(na.omit(stripCustomMissings(df[,grep(i, names(df))])))
    } else {
      df[,grep(i, names(df))]<- fun(df[,grep(i, names(df))])
    }
    names(df)[grep(i, names(df))]<- paste(names(df)[grep(i, names(df))], 'R', sep='')
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


#' recode defined missing codes back to NA for Row summing purposes
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
#' @param maxMissing (0 to 1) return a raw or prorated sum if the number of missings are under this threshold
#'
#' @param proRateMissing optionally prorate missings to produce a comparable sum if missings are allowed
#'
#' @importFrom haven labelled_spss
#' @importFrom labelled var_label
#' 
#' @return recoded df/dt

rowSumsCustomMissing<- function(df, customMissingCodes = c(-999,-888,-777,-666), missingValue = -666, maxMissing = 0, proRateMissings = FALSE) {
  # if the supplied DF is empty then we should return NULL so variables created using this function are not actually created
  if(ncol(df)==0 |nrow(df)==0) {
    warning("No data to sum - will not create this variable")
    return (NULL)
  }
  if(maxMissing >1 | maxMissing <0) { stop('Max missing is a proportion ( between 0 and 1 )') }
  na.rm<-ifelse(maxMissing==0, FALSE, TRUE)
  df<-stripCustomMissings(df, customMissingCodes)
  if(proRateMissings) {
    sums<-rowMeans(df, na.rm) * ncol(df)
  } else {
    sums<-rowSums(df, na.rm)
  }
  nas<-rowSums(is.na(df), na.rm=TRUE)
  sums[nas > maxMissing * ncol(df) ] <- missingValue
  Qlabel<-paste0("Sum of (", paste(names(df), collapse=','), ")")
  if(exists("customMissingValues")){
    sums <- labelled_spss(sums,
                  unlist(setNames(
                    customMissingValues, customMissingValueLabels
                  )),
                  label = Qlabel,
                  na_range = c(-999, -666))
  } else {
    labelled::var_label(sums)<-Qlabel
  }
  return(sums)
}


#' Utility function to calculate row means first stripping custom missings
#' and then replacing missing results with a custom missing
#' @param df Data Frame/Table to perform rowSums upon
#'
#' @param missingValue custom missing code to apply to missing results
#'
#' @param maxMissing (0 to 1) return a prorated sum if the number of missings are under this threshold
#'
#' @importFrom haven labelled_spss
#' @importFrom labelled var_label
#'
#' @return recoded df/dt
rowMeansCustomMissing<- function(df, customMissingCodes = c(-999,-888,-777,-666), missingValue = -666, maxMissing = 0) {
  # if the supplied DF is empty then we should return NULL so variables created using this function are not actually created
  if(ncol(df)==0 |nrow(df)==0) {
    warning("No data to make means from - will not create this variable")
    return (NULL)
  }
  if(maxMissing >1 | maxMissing <0) { stop('Max missing is a proportion ( between 0 and 1 )') }
  na.rm<-ifelse(maxMissing==0, FALSE, TRUE)
  df<-stripCustomMissings(df, customMissingCodes)
  means<-rowMeans(df, na.rm)
  nas<-rowSums(is.na(df), na.rm=TRUE)
  means[nas > maxMissing * ncol(df) ] <- missingValue
  Qlabel<-paste0("Mean of (", paste(names(df), collapse=','), ")")
  if(exists("customMissingValues")){
    means <- labelled_spss(means,
                  unlist(setNames(
                    customMissingValues, customMissingValueLabels
                  )),
                  label = Qlabel,
                  na_range = c(-999, -666))
  } else {
    labelled::var_label(means)<-Qlabel
  }
  return(means)
}



#' Utility function to strip html tags from string or vector
#' @param htmlString String /  String Vector to process
#' @return String / Vector with tags removed
stripHTML <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


#' Download Single Data File
#'
#' Download the specified task from the specified server
#' Authentication cache provided by authenticate function
#'
#' @param SMAusername username to login with
#' @param studyID Study to authenticate against
#' @param taskDigestID taskID AND digest ID (eg TASK_ID-DIGEST_ID)
#' @param server SMA server defaults to www.delosis.com
#' @param sampleID sampleID defaults to NULL
#' @keywords download dataset
#' @importFrom data.table fread
#' @import R.utils
#'
#' @export
downloadSingleDataFile<-function(SMAusername, studyID, taskDigestID, server="www.delosis.com", sampleID=NULL){
  #prompt for password if we don't hold it in the current session
  login <- DelosisAuthenticate(SMAusername, studyID, server)
  if(is.null(login)) {stop("Authentication Cancelled")}
  URL<-paste('https://', URLencode(login["username"], reserved=T), ':',URLencode(login["password"], reserved=T), '@', login["server"], '/psytools-server/dataservice/dataset/', sep='')
  taskID<-paste(studyID,taskDigestID, sep='-')
  if(!is.null(sampleID)) {taskID<-paste(taskID, sampleID, sep='-')}
  URL<-URLencode(paste(URL, taskID, '.csv.gz', sep=''))

  dt<-NULL
  retries<-0
  while(is.null(dt) && retries<3) {
    try(
      dt<-data.table::fread(URL ,stringsAsFactors=FALSE, blank.lines.skip=TRUE, encoding="UTF-8")
    )
    retries<-retries+1
    Sys.sleep(2)
  }

  if (!is.null(dt)) {
    if(nrow(dt)>0) {
    ##replace spaces and [] in column names to preserve compatibility with read.table
    colnames(dt)<-gsub('[] []','.', colnames(dt))
    return(dt)
    } else {
      warning(paste(taskID, 'is empty - returning an empty dt'))
      return(dt)
    }
  }
  else {
    warning(paste("Could not download dataset", taskID, "from server", server, "using SMA username", login["username"]), call.=FALSE)
    #try again perhaps the password was wrong
    login <- DelosisAuthenticate(SMAusername, studyID, server, TRUE)
    if(is.null(login)) {stop("Authentication Cancelled")}
    return(downloadSingleDataFile(SMAusername, studyID, taskDigestID, server, sampleID))
  }
}

#' Authenticate
#' local authentication cache prompts for login in shiny window and will cache details for a study and server over repeated calls.
#'
#' @param SMAusername Username to access SMA
#' @param studyID Study ID
#' @param server SMA server defaults to www.delosis.com
#' @param resetCache Force a new prompt defaults to FALSE
#' @importFrom askpass askpass
#' @keywords authentication download dataset
DelosisAuthenticate<-function(SMAusername, studyID, server="www.delosis.com", resetCache=FALSE) {
  if (!resetCache &
      !is.null(Sys.getenv("SMAusername")) &&
      SMAusername == Sys.getenv("SMAusername") &&
      !is.null(Sys.getenv("SMAstudyID")) &&
      studyID == Sys.getenv("SMAstudyID") &&
      !is.null(Sys.getenv("SMAserver")) &&
      server == Sys.getenv("SMAserver") &&
      !is.null(Sys.getenv("SMApassword"))) {
    return(c(username=SMAusername,password=Sys.getenv("SMApassword"),server=server))
  }
  else {
    PASSWORD<-askpass::askpass(paste("Delosis SMA password for", SMAusername, " on ", server))
    if (!is.null(PASSWORD)){
      login<-c(username=SMAusername,password=PASSWORD,server=server)
      Sys.setenv("SMAusername"= SMAusername)
      Sys.setenv("SMAserver" = server)
      Sys.setenv("SMAstudyID" = studyID)
      Sys.setenv("SMApassword" = PASSWORD)
      return(login)
    }
    else {return(NULL)}
  }
}

#' Label Data Frame from Psytools (Desktop) Questionnaire Resources file
#' @param df Wide format data frame (or table) to label
#' 
#' @param resources df containing Psytools resources for this instrument
#' 
#' @importFrom haven labelled_spss
#' 
#' @importFrom labelled var_label
#' 
#' @export
#' 
labelData<-function(df, resources) {
  # Don't even try if it doesn't look at least a bit like a questionnaire resources sheet
  if(ncol(resources) <5 | length(resources[grepl('%%', resources)])==0){
    return(df)
  }
  #remove any rows where there is nothing in the QCode column
  resources<-resources[!is.na(resources[1]),]
  # remove any rows where there are no responses specified as these will generate no data
  resources<-resources[rowSums(!is.na(resources[5:ncol(resources)]))>1, ]

  #Create a single variable label from Title and Question
  Rlabels<-list()
  apply(resources, 1, function(x) {
    Qcode <-as.character(x[1])
    isAllThatApply <-
      ifelse(grepl('allthatapply', x[5], ignore.case = TRUE), TRUE, FALSE)
    if(!Qcode %in% names(df)) {
      # Perhaps it was reversed in the derivations
      if(paste0(Qcode, "R") %in% names(df)) {
        Qcode<-paste0(Qcode, "R")
      } else {
        # - There are a lot of these! warning(paste0(Qcode, ' specified in resources but not in data'))
        return()
      }
    }
    Qlabel <- gsub("NA : | : NA", "", paste(stripHTML(x[4]), stripHTML(x[3]), sep=' : '))
    Rlabels<<-list()
    Rlabel<-strsplit(as.character(x[grepl('%%', x[5:length(x)])]), "%%")
    if(length(Rlabel)){
        lapply(Rlabel, function(responseLabel) {
          responseLabel[1]<-gsub('\\*\\*NA\\*\\*|other_specify', '', responseLabel[1])
          responseLabel[2]<-stripHTML(responseLabel[2])
          if(responseLabel[1] != '') {
            Rlabels<<-c(Rlabels, setNames(responseLabel[1], responseLabel[2]))
          }
        })
    }
    if(isAllThatApply==TRUE) {
        lapply(Rlabels, function(responseLabel) {
          subVariable<-paste0(Qcode, '_', responseLabel[1])
          if(!subVariable %in% names(df)) {
            if(paste0(subVariable, "R") %in% names(df)) {
              subVariable<-paste0(subVariable, "R")
            } else {
              return()
            }
          }
          subVariableLabel <-
            paste0(names(Rlabels)[which(Rlabels == responseLabel)], ' : ', Qlabel)
          df[,subVariable]<<-
            labelVariable(df[,subVariable],
                          as.list(setNames(c(0,1), c('No','Yes'))),
                          subVariableLabel
                          
            )
        })
      }else{
        df[,Qcode]<-labelVariable(df[,Qcode], Rlabels, Qlabel)
      }
    df<<-df})
  return(df)
}

#' Label individual row - split out as seperate function to simplify loops in labelData
#' @param x vector to label
#' @param Rlabels named list to apply to response labels
#' @param Qlabel String to apply as question label
#' @importFrom haven labelled_spss
#' @importFrom labelled var_label
#' 
labelVariable <- function (x, Rlabels, Qlabel) {
  #strip out non numeric response labels from numeric variables - they will never be used and are not supported
  if(("numeric" %in% class(x) | is.numeric(x)) & !is.null(Rlabels)){
    suppressWarnings({
      Rlabels<-Rlabels[!is.na(as.numeric(Rlabels))]
      Rlabels<-setNames(as.numeric(Rlabels), names(Rlabels))
    })
  }
  if(exists("customMissingValues") & !is.null(Rlabels)){
    if("numeric" %in% class(x) | is.numeric(x)) {
      missingLabels <-
        as.list(setNames(customMissingValues,
                         customMissingValueLabels))
    } else {
      missingLabels <-
        as.list(setNames(as.character(customMissingValues),
                         customMissingValueLabels))
    }
    Rlabels<-c(Rlabels, missingLabels)
  }
  # For the SDIM There is a duplicate response code (13 gradute and postgraduate )
  # This must be coerced in the database but for now just assign the value 14 to it in the resources sheet

  if("numeric" %in% class(x) | is.numeric(x)){
    x<-labelled_spss(x,
                     unlist(Rlabels),
                     label = Qlabel,
                     #SPSS only support 3 missing values for non numeric variables... 
                     na_range = c(-999,-666)
    )
  } else{
  x<-labelled_spss(x,
                  unlist(Rlabels),
                  label = Qlabel,
                  na_values = c('-888', '-777', '-666')
  )
  }
  return(x)
}


#' Convert new style AllThatApply (seperate binary sufixed columns) into old style (single column concatenated with pipe )
#' @param df data frame to work on
#' @param grepColumnCollection a grepable term to grab all columns that should be merged
#' @param finalColumn the new column name to contain the merged data
#' @param booleanIndicator - defaults to "Y"
#' @importFrom data.table setDT
#' @importFrom data.table setDF
MergeAllThatApply<- function (df, grepColumnCollection, finalColumn, booleanIndicator ="Y") {
  setDT(df)
  targetCols<-grep(grepColumnCollection, names(df))

  # replace all "Y" with the column name suffix ( between the periods ( originally ))
  for(col in targetCols) {
    df <-
      df[as.vector(df[, ..col] ==booleanIndicator), 
         (col) := gsub("^[A-z0-9]+[\\.[]|[\\.]]$", "", names(df)[col]
                      )
         ]
  }
  # set the first column to be the allThatApply column and remove the rest
  df[,(targetCols[1]) := apply(
    df[, grepl(grepColumnCollection, names(df)), with = FALSE],
    1,
    function(x) paste(x[x!="" & !is.na(x)], collapse="|")
  )]
  names(df)[targetCols[1]]<-finalColumn
  targetCols<-targetCols[2:length(targetCols)]
  df[,(targetCols) := NULL]
  return(setDF(df))
} 



