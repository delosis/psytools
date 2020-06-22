# Additional functions for the IMAGEN study
#
#  Copyright (C) 2017-2020 Delosis
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
deriveImagenCTS <- function(df) {
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
deriveImagenReliability <- function(df) {
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
deriveImagenADRS <- function(df) {
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


#' Generate summary for CSI questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form CSI data
#'
#' @return wide form of CSI data with summary vars
#'
#' @export
deriveImagenCSI <- function(df) {
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
deriveImagenIRI <- function(df) {
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
deriveImagenPDS <- function(df) {
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
deriveImagenAUDIT <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  #Recode items 9 and 10 as per Frauke's comments
  df$audit9[df$audit9==2] <- 4
  df$audit9[df$audit9==1] <- 2
  df$audit10[df$audit10==2] <- 4
  df$audit10[df$audit10==1] <- 2

  #Summary Vars
  df$audit_freq <-
    rowSums(cbind(df$audit1, df$audit2, df$audit3), na.rm = TRUE)
  df$audit_symp <-
    rowSums(cbind(df$audit4, df$audit6, df$audit8), na.rm = TRUE)
  df$audit_prob <-
    rowSums(cbind(df$audit5, df$audit7, df$audit9, df$audit10), na.rm = TRUE)
  df$audit_total <-
    rowSums(cbind(df$audit_freq, df$audit_symp, df$audit_prob), na.rm = TRUE)
  #Added abuse flag as per Frauke's comments
  df$audit_abuse_flag <- ifelse(df$audit_total>7,1,0)
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
deriveImagenMAST <- function(df) {
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
#' @param requiresReverseCoding (default FALSE) boolean indicating if the df is already reverse coded - If not the R suffix is still applied to the reversed items (if not there already) to avoid confusion
#'
#' @return wide form of NEO FFI data with summary vars
#'
#' @export
deriveImagenNEO <- function(df, requiresReverseCoding = FALSE) {
  #Rotate
  df <- rotateQuestionnaire(df)

  if (requiresReverseCoding) {
    df <-
      recodeVariables(
        df,
        paste0(names(df)[grepl(
          "[^0-9_](1|3|8|9|12|14|15|16|18|23|24|27|29|30|31|33|38|39|42|44|45|46|48|54|55|57|59)$",
          names(df)
        )], "$"),
        function(x) {
          4 - x
        }
      )
  } else {
    names(df)[grepl(
      "[^0-9_](1|3|8|9|12|14|15|16|18|23|24|27|29|30|31|33|38|39|42|44|45|46|48|54|55|57|59)$",
      names(df)
    )] <-
      paste0(names(df)[grepl(
        "[^0-9_](1|3|8|9|12|14|15|16|18|23|24|27|29|30|31|33|38|39|42|44|45|46|48|54|55|57|59)$",
        names(df)
      )], "R")
  }

  #Summaries
  df$neur_mean <-
    rowMeans(df[, grepl("[^_][16]R?$", colnames(df))])
  df$extr_mean <-
    rowMeans(df[, grepl("[^_][27]R?$", colnames(df))])
  df$open_mean <-
    rowMeans(df[, grepl("[^_][38]R?$", colnames(df))])
  df$agre_mean <-
    rowMeans(df[, grepl("[^_][49]R?$", colnames(df))])
  df$cons_mean <-
    rowMeans(df[, grepl("[^_][50]R?$", colnames(df))])
  return(df)
}




#' Generate summary for TCI questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form TCI data
#'
#' @param requiresReverseCoding (default FALSE) boolean indicating if the df is already reverse coded - If not the R suffix is still applied to the reversed items (if not there already) to avoid confusion
#'
#' @return wide form of TCI data with summary vars
#'
#' @export
deriveImagenTCI <- function(df, requiresReverseCoding=FALSE) {
  #Rotate
  df <- rotateQuestionnaire(df)
  if (requiresReverseCoding) {
    df <-
      recodeVariables(
        df,
        paste0(names(df)[grepl(
          "(222|014|047|059|071|053|105|123|139|145|155|156|159|165|170|172|176|179|193|205|210|239)$",
          names(df)
        )], "$"),
        function(x) {
          6 - x
        }
      )
  } else {
    names(df)[grepl(
      "(222|014|047|059|071|053|105|123|139|145|155|156|159|165|170|172|176|179|193|205|210|239)$",
      names(df)
    )] <-
      paste0(names(df)[grepl(
        "(222|014|047|059|071|053|105|123|139|145|155|156|159|165|170|172|176|179|193|205|210|239)$",
        names(df)
      )], "R")
  }
  #Summaries
  df$tci_excit <-
    rowSums(df[, grepl("(001|063|053|104|122|145|156|165|176|205)R?", colnames(df))])
  df$tci_imp <-
    rowSums(df[, grepl("(010|047|071|102|123|179|193|210|239)R?", colnames(df))])
  df$tci_extra <-
    rowSums(df[, grepl("(014|024|059|105|139|155|172|215|222)R?", colnames(df))])
  df$tci_diso <-
    rowSums(df[, grepl("(044|051|077|109|135|159|170)R?", colnames(df))])
  df$tci_novseek <-
    rowSums(df[, grepl("tci_", colnames(df))])
  return(df)
}

#' Generate summary for TCI3 questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' The TCI3 had different items in the German FU2 version - for FU3 the German was updated to match the English and French Locales
#' It seems from discussions with Frauke that actually the German version was the originally intended one and has the most subscales derivable from it
#' all are merged together for return
#'
#' The derived varaibles produces depend on which
#'
#' @param df data frame containing long form TCI3 data
#'
#' @return wide form of TCI3 data with summary vars
#'
#' @importFrom data.table rbindlist setDF
#'
#' @export
deriveImagenTCI3 <- function(df) {
  # Split out participants who have done the FU2 DE style list of questions as there is item number over lap
  # TODO it might be wise to homogneise the item numbers to whatever is "correct"

  dfdefu2 <- df[paste0(df$User.code,'-', df$Iteration) %in% do.call(paste, c(df[df$Trial=='TCI_4', 1:2], sep = "-")), ]
  df <- df[!(paste0(df$User.code,'-', df$Iteration) %in% do.call(paste, c(df[df$Trial=='TCI_4', 1:2], sep = "-"))), ]

  #Rotate
  df <- rotateQuestionnaire(df)
  names(dfdefu2)[grepl('TCI', names(dfdefu2))] <- paste("DEFU2",names(dfdefu2)[grepl('TCI', names(dfdefu2))],  sep="_")

  # Recode
  df <-
    recodeVariables(
      df,  paste0("[_]", c('96', '110','79'), "R?$"),
      function(x) {
        6 - x
      }
    )

  #Summaries
  df$tci_rs_binding <-
    rowSums(df[, grepl("[_](96|116|110|15|79)R?$", colnames(df))])
  df$tci_i_ambition <-
    rowSums(df[, grepl("[_](60|117|37|126|62)R?$", colnames(df))])

  # If there are any DEFU2 rows then treat them seperately
  if (nrow(dfdefu2)) {
    dfdefu2 <- rotateQuestionnaire(dfdefu2)
    dfdefu2 <-
      recodeVariables(
        dfdefu2,
        paste0(
          "[_]",
          c('108', '129', '8', '80', '123', '107', '114', '118', '128', '133'),
          "R?$"
        ),
        function(x) {
          6 - x
        }
      )
    dfdefu2$tci_rs_binding <-
      rowSums(dfdefu2[, grepl("[_](20|26|44|99|119)R?$", colnames(dfdefu2))])
    dfdefu2$tci_i_ambition <-
      rowSums(dfdefu2[, grepl("[_](18|35|67|111|139)R?$", colnames(dfdefu2))])
    dfdefu2$tci_rs_sensitivity <-
      rowSums(dfdefu2[, grepl("[_](30|56|71|78|137)R?$", colnames(dfdefu2))])
    dfdefu2$tci_rs_emoopenness <-
      rowSums(dfdefu2[, grepl("[_](39|104|108|112|129)R?$", colnames(dfdefu2))])
    dfdefu2$tci_rs_dependency <-
      rowSums(dfdefu2[, grepl("[_](8|75|80|96|123)R?$", colnames(dfdefu2))])
    dfdefu2$tci_i_workenthusiasm <-
      rowSums(dfdefu2[, grepl("[_](107|114|118|128|133)R?$", colnames(dfdefu2))])
    dfdefu2$tci_i_workinghard <-
      rowSums(dfdefu2[, grepl("[_](1|40|57|98|136)R?$", colnames(dfdefu2))])
    dfdefu2$tci_i_perfectionism <-
      rowSums(dfdefu2[, grepl("[_](5|25|54|77|88)R?$", colnames(dfdefu2))])
    df <- rbindlist(list(df, dfdefu2), fill = TRUE)
  }

  return(setDF(df))
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
deriveImagenESPAD <- function(df) {
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
#' @importFrom data.table dcast setDT setDF
#'
#' @export
deriveImagenGEN <- function(df) {
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
#' @importFrom data.table dcast setDT setDF
#'
#' @export
deriveImagenIDENT <- function(df) {
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
  return(dfsums)
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
#' @importFrom data.table dcast setDT setDF
#'
#' @export
deriveImagenDOTPROBE <- function(df) {
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
  return(dfsums)
}


#' Convert FU3 to FU2 data format
#'
#' Convert FU3 variable names and any Value recodes neccessary
#' to return to the format expected from Imagen FU2
#'
#' @param df Data frame with FU3 data for an instrument or instruments represented by a single task at FU2
#'
#' @return named list of Data Frames in FU2 format representing the component FU2 instruments contained in the FU3
#'
#' @importFrom data.table dcast setDT setDF setcolorder setorder set
#'
#' @export
#'
convertFU3toFU2 <- function(df) {
  setDT(df)
  fu3Names <- as.data.table(names(df))
  names(fu3Names) <- "fu3Column"
  nameMap <- merge(fu3Names,imagenFu2Fu3Map, by="fu3Column", all.x = TRUE)
  
  # If there is data in the df which does not have any defined instrument to belong to then create a pseudo instrument "EXTRA"
  nameMap[is.na(nameMap$fu2Column)]$Instrument <- "EXTRA"
  nameMap[is.na(nameMap$fu2Column)]$fu2Column <- nameMap[is.na(nameMap$fu2Column)]$fu3Column
  
  # but strip out the token submitdate and startdate variables 
  nameMap<-nameMap[nameMap$fu2Column != 'token' & 
                     nameMap$fu2Column != 'startdate' & 
                     nameMap$fu2Column != 'submitdate' & 
                     nameMap$fu2Column != 'lastpage', ]
  

  #Swap the FU3 names for FU2 names
  for (i in 1:nrow(nameMap)) {
    if (as.character(nameMap$fu2Column[i])=="DELETE") {
      df[,as.character(nameMap$fu3Column[i])] <- NULL
    } else {
      names(df)[which(names(df) == as.character(nameMap$fu3Column[i]))] <- as.character(nameMap$fu2Column[i])
    }
  }
  nameMap <- nameMap[fu2Column!="DELETE",]

  #collapse All That Applies into a single column as at FU2
  allThatApplys <- imagenFu2Fu3Map[grepl('AllThatApply',imagenFu2Fu3Map$fu3Column),]
  for (i in 1:nrow(allThatApplys)) {
    grepColumnCollection <- gsub('.AllThatApply','',allThatApplys[i,1])
    if (length(grep(grepColumnCollection, names(df)))) {
      df <- MergeAllThatApply(df,grepColumnCollection, as.character(allThatApplys[i,2]))
    }
  }

  Instruments <- unique(as.character(nameMap[,Instrument]))
  Instruments <- Instruments[!Instruments %in% c("ALL", "NONE")]
  Instruments <- Instruments[!is.na(Instruments)]
  
  splitDFs <- list()
  #Split and return list of DTs
  for (targetInstrument in Instruments) {
     #print(targetInstrument)
     targetVars <- as.character(nameMap[Instrument==targetInstrument | Instrument=="ALL", fu2Column])
     targetDT <- as.data.table(df[, names(df) %in% targetVars, with=FALSE])
     #Update the completed flag
     # set to complete if they completed the whole bundle
     targetDT$Completed[!is.na(targetDT$Completed)] <- 't'
     # else use the instrument specific logic
     targetDT$Completed[targetDT$Completed != 't'] <- getFU3Complete(targetInstrument,targetDT[targetDT$Completed != 't',])
     #If there is no iteration provided then create one based on completion time
     if(is.null(targetDT$Iteration)) {
       targetDT[, Iteration := seq(.N), by = c("User.code", "Completed.Timestamp")]
     }
     targetDT$Processed.Timestamp <- targetDT$Completed.Timestamp
     setcolorder(targetDT, c("User.code", "Iteration", "Language", "Completed", "Completed.Timestamp", "Processed.Timestamp"))
     #Coerce all value columns and Trial to character to avoid warnings when melting
     for (col in names(targetDT)[(which(names(targetDT) == "Processed.Timestamp") + 1):length(names(targetDT))]) {
       set(targetDT, j=col, value=as.character(targetDT[[col]]))
     }
     targetDT <- melt.data.table(targetDT,
                    id.vars = names(targetDT)[1:which(names(targetDT)=="Processed.Timestamp")],
                    measure.vars = names(targetDT)[(which(names(targetDT)=="Processed.Timestamp") + 1):length(names(targetDT))],
                    variable.name = "Trial",
                    value.name = "Trial.result",
                    variable.factor = FALSE
                    )
     targetDT$Block <- targetDT$Trial
     setcolorder(
       targetDT,
       c(
         "User.code",
         "Iteration",
         "Language",
         "Completed",
         "Completed.Timestamp",
         "Processed.Timestamp",
         "Block",
         "Trial"
       )
     )
     setorder(targetDT, User.code)
     targetDT <- getFU3Recode(targetInstrument, targetDT)
     splitDFs[[targetInstrument]] <- targetDT
  }

  return(splitDFs)
}

getFU3Recode <- function(targetInstrument, targetDT) {
  # Convert Y/N to 1/0
  targetDT[grepl(
    "ts_2|leq_[0-9][0-9]_ever|HRQOL_ALM_1|^mast[0-9]+[ab]?$|SCAMP_01|SCAMP_06|SCAMP_07|EDEQ_31|EDEQ_33|^23$|^31|^SCID_A1AC_[135]_|SCID_A1B_[1345]_|SCID_A1D_[1346]_|SCID_A[23]_[135]_|SCID_A4ABCD_[135]_|SCID_D1AB_[13]_|SCID_D2A_1_|SCID_D2B_|SCID_D3AB_|SCID_D4AB_[13567]_|SCID_D5_[12]_|SCID_D6_|SCID_D7ABCD_[13456]_",
    Trial
  ), Trial.result := ifelse(Trial.result == "Y",
                            "1",
                            ifelse(
                              Trial.result == "N",
                              "0",
                              ifelse(Trial.result == "", NA, Trial.result)
                            ))]
  switch(
      targetInstrument,
      "IMGN_KIRBY_FU3"= targetDT[grepl('kirby',Trial), Trial.result:= as.character(ifelse(targetDT[grepl('kirby',Trial),Trial.result]=="NOW", '0','1'))],
      "IMGN_HRQOL_FU3"= targetDT[Trial=='HRQOL_ALM_2' & Trial.result=="-oth-", Trial.result := as.character(14)],
      targetDT
    )
}


getFU3Complete <- function(targetInstrument, targetDT) {
  ifelse(is.na(targetDT$submitdate),
    switch(
      targetInstrument,
      "IMGN_ANXDX_FU3" = ifelse(rowSums(is.na(targetDT[,(which(names(targetDT)=="ts_4")+ 1):which(names(targetDT)=="ANXDX_17_EVER"), with=FALSE])), 'f','t'),
      "IMGN_BSI_FU3" = ifelse(targetDT$BSICheck.change.=="N" & targetDT$BSICheck.truth.=="Y", 't','f'),
      "IMGN_CAPE_FU3" = ifelse(targetDT$CAPECheck.change.=="N" & targetDT$CAPECheck.truth.=="Y", 't','f'),
      "IMGN_ESPAD_FU3" = ifelse(targetDT$EspadCheck.change.=="N" & targetDT$EspadCheck.truth.=="Y", 't','f'),
      "IMGN_SURPS_FU3" = ifelse(targetDT$surpsCheck.change.=="N" & targetDT$surpsCheck.truth.=="Y", 't','f'),
      "IMGN_AUDIT_FU3" = ifelse(targetDT$audit1==0 | !rowSums(is.na(targetDT[,(which(names(targetDT)=="ts_4")+ 1):ncol(targetDT), with=FALSE])), 't','f'),
      "IMGN_EDEQ_FU3" = ifelse(rowSums(is.na(targetDT[,(which(names(targetDT)=="ts_4")+ 1):ncol(targetDT), with=FALSE])) >5 , 'f','t'),
      "IMGN_HRQOL_FU3" = ifelse(!is.na(targetDT$HRQOL_HDSM_5), 't','f'), # TODO Could be improved
      "IMGN_K6PLUS_FU3" = ifelse(rowSums(targetDT[,grepl('K6PLUS_1', names(targetDT)), with=FALSE], na.rm=TRUE) ==30 | !is.na(targetDT$K6PLUS_6), 't','f'),
      "IMGN_LEQ_FU3" = ifelse(rowSums(is.na(targetDT[,grepl("_ever", names(targetDT)), with=FALSE])), 'f','t'),
      "IMGN_VIDGAME_FU3" = ifelse(targetDT$VideoGame_1==0 | rowSums(is.na(targetDT[,(which(names(targetDT)=="ts_4")+ 1):ncol(targetDT), with=FALSE]))<7, 't','f'), # TODO could be improved
      # check if there are any NAs in all columns beyond ts_4 - suitable for all compulsory instruments
      ifelse(rowSums(is.na(targetDT[,(which(names(targetDT)=="ts_4")+ 1):ncol(targetDT), with=FALSE])), 'f','t')
    )
  ,'t')
}
