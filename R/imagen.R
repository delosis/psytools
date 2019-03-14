# Additional functions for the IMAGEN study
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
#' @param df data frame containing long form IRI data
#'
#' @return wide form of IRI data with summary vars
#'
#' @export
deriveImgnIRI <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  #Summary
  df$IRI_fantasy <-
    rowSums(df[, grepl("I01|I05|I07|I12|I16|I23|I26", colnames(df))], na.rm =
              TRUE)
  df$IRI_perspective <-
    rowSums(df[, grepl("I03|I08|I11|I15|I21|I25|I28", colnames(df))], na.rm =
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
    rowMeans(df[, grepl("[^_][16]$", colnames(df))])
  df$extr_mean <-
    rowMeans(df[, grepl("[^_][27]$", colnames(df))])
  df$open_mean <-
    rowMeans(df[, grepl("[^_][38]$", colnames(df))])
  df$agre_mean <-
    rowMeans(df[, grepl("[^_][49]$", colnames(df))])
  df$cons_mean <-
    rowMeans(df[, grepl("[^_][50]$", colnames(df))])
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
  df$h_mean <-
    rowMeans(df[, grepl("s1$|s4|s7|s13|s17|s20|s23", colnames(df))])
  df$as_mean <-
    rowMeans(df[, grepl("s8|s10|s14|s18|s21", colnames(df))])
  df$imp_mean <-
    rowMeans(df[, grepl("s2$|s5|s11|s15|s22", colnames(df))])
  df$ss_mean <-
    rowMeans(df[, grepl("s3$|s6|s9|s12|s16|s19", colnames(df))])
  df$h_sum <-
    rowSums(df[, grepl("s1$|s4|s7|s13|s17|s20|s23", colnames(df))])
  df$as_sum <-
    rowSums(df[, grepl("s8|s10|s14|s18|s21", colnames(df))])
  df$imp_sum <-
    rowSums(df[, grepl("s2$|s5|s11|s15|s22", colnames(df))])
  df$ss_sum <-
    rowSums(df[, grepl("s3$|s6|s9|s12|s16|s19", colnames(df))])
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
  df <- df[df$Response != 'exit_button',]
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
    rotateQuestionnaire(df[substr(df$Block, 1, 8) != "Relation", ])
  #remove undefined relations ( there will always be one at the end )
  #Also remove the disgnosis and sure rows - not needed now we have the variables
  df <-
    df[df$Relation != "" &
         df$Trial != "sure" & df$Trial != "disorder",]
  df <- df[!is.na(df$User.code),]
  
  #Order by relation within iteration
  df <- df[order(df$User.code, df$Iteration, df$Relation),]
  ## Iterate through and subscript the relation with an index so multiple diagnoses per relation can be supported
  df$RelationOrderIdx <- 1
  for (i in 1:nrow(df)) {
    if (i > 1) {
      if (df[i,]$User.code == df[i - 1,]$User.code &
          df[i,]$Iteration == df[i - 1,]$Iteration &
          df[i,]$Relation == df[i - 1,]$Relation) {
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
              substr(df$Block, 1, 3) == 'id_',]) > 0) {
    withTSID <- TRUE
    dfTsID <-
      rotateQuestionnaire(df[substr(df$Block, 1, 3) == 'ts_' |
                               substr(df$Block, 1, 3) == 'id_',])
    df <-
      df[!(substr(df$Block, 1, 3) == 'ts_' |
             substr(df$Block, 1, 3) == 'id_'),]
  }
  
  #Select just the Main block
  df <- df[df$Block == 'IDENT_MAIN',]
  
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
              substr(df$Block, 1, 3) == 'id_',]) > 0) {
    withTSID <- TRUE
    dfTsID <-
      rotateQuestionnaire(df[substr(df$Block, 1, 3) == 'ts_' |
                               substr(df$Block, 1, 3) == 'id_',])
    df <-
      df[!(substr(df$Block, 1, 3) == 'ts_' |
             substr(df$Block, 1, 3) == 'id_'),]
  }
  
  #Select just the Main block
  df <- df[df$Block == 'DOT_PROBE_MAIN',]
  
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
  dfsums <- dfsums[,!grepl("RT_sum|SCORE_mean", colnames(dfsums))]
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
