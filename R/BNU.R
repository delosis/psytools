# Additional functions for the BNU studies
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



#' Generate summary for PDS questionnaire as administered to children in BNU
#'
#' NB - it is NOT suitable for the Adult version as that asks completely differnt questions - no derivation has been defined for that
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form PDS data
#'
#' @return wide form of PDS data with summary vars
#'
#' @export
deriveBnuPDS <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  # This does not work for the adult version - just return the rotated Q
  if("T1_PD_BOY_A1" %in% names(df)) {
    return(df)
  }

  # Summary Note allowing missings as there are seperate variables for boys and girls - no prorating
  # Boys is 2 4 and 6 summed
  df$PDS_sum[df[,grepl('gender',names(df))]=='1'] <-
               rowSumsCustomMissing(df[df[,grepl('gender',names(df))]=='1', grepl("2|4|5", colnames(df))],
                                    maxMissing = 1,
                                    proRateMissings = FALSE)
  # Girls is just Q2 and 4 ( Hair and Breasts ) Q6 is incorporated for the stage variable
  df$PDS_sum[df[,grepl('gender',names(df))]=='2'] <-
               rowSumsCustomMissing(df[df[,grepl('gender',names(df))]=='2', grepl("2|4", colnames(df))],
                                    maxMissing = 1,
                                    proRateMissings = FALSE)


  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 12]<-5
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 9 & df$PDS_sum <= 11]<-4
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 6 & df$PDS_sum <= 8]<-3
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 4 & df$PDS_sum <= 5]<-2
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum < 4]<-1

  df$PDS_stage[df[,grepl('gender',names(df))]=='2' & df$T1_PD_GIRL_C05==1 & df$PDS_sum>=8]<-5
  df$PDS_stage[df[,grepl('gender',names(df))]=='2' & df$T1_PD_GIRL_C05==1 & df$PDS_sum < 8]<-4
  df$PDS_stage[df[,grepl('gender',names(df))]=='2' & df$T1_PD_GIRL_C05==2 & df$PDS_sum > 3]<-3
  df$PDS_stage[df[,grepl('gender',names(df))]=='2' & df$T1_PD_GIRL_C05==2 & df$PDS_sum == 3]<-2
  df$PDS_stage[df[,grepl('gender',names(df))]=='2' & df$PDS_sum < 3]<-1
  return(df)
}

#' Generate summary for SC questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form SC data
#'
#' @return wide form of SC data with summary vars
#'
#' @export
deriveBnuSC <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  # reverse code
  reverseVariables <- c('05','09',13,17,21,22,27,28,30,34,35,38)
  df<-recodeVariables(df, reverseVariables, fun= function(x) {5-x})

  #Summary
  df$order_and_discipline <-
    rowMeansCustomMissing(df[, grepl("01|05|09|13|17|21|25|29|33", colnames(df))])
  df$acceptance_and_support <-
    rowMeansCustomMissing(df[, grepl("02|06|10|14|18|22R|26|30|34", colnames(df))])
  df$fairness_and_justice <-
    rowMeansCustomMissing(df[, grepl("03|07|11|15|19|23|27|31|35", colnames(df))])
  df$encouragement_and_cooperation <-
    rowMeansCustomMissing(df[, grepl("04|08|12|16|20|24|28|32|36", colnames(df))])
  df$school_beliefs <-
    rowMeansCustomMissing(df[, grepl("37|38|39|40", colnames(df))])

  return(df)
}


#' Generate summary for COPE questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form COPE data
#'
#' @return wide form of COPE data with summary vars
#'
#' @export
deriveBnuCOPE <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  #Summary
  df$Positive_coping <-
    rowMeansCustomMissing(df[, grepl("01|02|03|04|05|06|07|08|09|10|11|12", colnames(df))])
  df$Negative_coping <-
    rowMeansCustomMissing(df[, grepl("13|14|15|16|17|18|19|20", colnames(df))])

  message('Specs ask for a norming score - do you want to use Z scores within each dataset here?')
  return(df)
}



#' Generate summary for CWB questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form CWB data
#'
#' @return wide form of CWB data with summary vars
#'
#' @export
deriveBnuCWB <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  #Summary
  df$Emotional_index <-
    rowMeansCustomMissing(df[, grepl("1|2|3|4|5|6|7|8", colnames(df))])
  df$Life_satisfaction <- df[, grepl("9", colnames(df))]
  df$Student_happiness <- df$Emotional_index + 1.1* df$Life_satisfaction

  return(df)
}

#' Generate summary for ER questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form ER data
#'
#' @return wide form of ER data with summary vars
#'
#' @export
deriveBnuER <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  #Summary
  df$Reappraisal <-
    rowSumsCustomMissing(df[, grepl("1$|3|5|7|8|10", colnames(df))])
  df$Suppression <-
    rowSumsCustomMissing(df[, grepl("2|4|6|9", colnames(df))])

  return(df)
}

#' Generate summary for BISBAS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form BISBAS data
#'
#' @return wide form of BISBAS data with summary vars
#'
#' @export
deriveBnuBISBAS <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  # reverse code
  reverseVariables <- c('02','22')
  df<-recodeVariables(df, reverseVariables, fun= function(x) {5-x})

  #Summary
  df$BAS_Drive <-
    rowSumsCustomMissing(df[, grepl("03|09|12|21", colnames(df))])
  df$BAS_Fun_seeking <-
    rowSumsCustomMissing(df[, grepl("05|10|15|20", colnames(df))])
  df$BAS_Reward_responsivness <-
    rowSumsCustomMissing(df[, grepl("04|07|14|18|23", colnames(df))])
  df$BIS <-
    rowSumsCustomMissing(df[, grepl("02R|08|13|16|19|22R|24", colnames(df))])

  return(df)
}

#' Generate summary for OCD questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form OCD data
#'
#' @return wide form of OCD data with summary vars
#'
#' @export
deriveBnuOCD <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  # recode down by 1
  recodeVariables <- names(df)[grepl('OCD[^specify]*$', names(df))]
  df<-recodeVariables(df, paste0(recodeVariables, '$'), fun= function(x) {x-1})

  #Summary
  df$OCD_sum <-
    rowSumsCustomMissing(df[, grepl('OCD[^specify]*$', names(df))])

  return(df)
}

#' Generate summary for Motivation and wellbeing-70 (MW70) questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form PDS data
#'
#' @return wide form of PDS data with summary vars
#'
#' @export
deriveBnuMW70 <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  # reverse code
  reverseVariables <- c('08', '10', 12, 46, 45, 62, 65, '[^MW]70', 63, 67, 68)
  df<-recodeVariables(df, reverseVariables, fun= function(x) {6-x})

  #Summary
  df$Fear_of_fauilure <-
    rowMeansCustomMissing(df[, grepl("01|02|03", colnames(df))])
  df$Academic_withdrawal <-
    rowMeansCustomMissing(df[, grepl("04|05|06", colnames(df))])
  df$Self_esteem <-
    rowMeansCustomMissing(df[, grepl("07|08|09|10|11", colnames(df))])
  df$School_value <-
    rowMeansCustomMissing(df[, grepl("12|43|44|45|46|47", colnames(df))])
  df$Mastery_intrinsic <-
    rowMeansCustomMissing(df[, grepl("13|14|15", colnames(df))])
  df$Mastery_extrinsic <-
    rowMeansCustomMissing(df[, grepl("16|17|18", colnames(df))])
  df$Performance_approach <-
    rowMeansCustomMissing(df[, grepl("19|20|21", colnames(df))])
  df$Performance_avoidance <-
    rowMeansCustomMissing(df[, grepl("22|23|24", colnames(df))])
  df$Avoidance <-
    rowMeansCustomMissing(df[, grepl("25|26|27", colnames(df))])
  df$Agency_effort <-
    rowMeansCustomMissing(df[, grepl("28|29|30", colnames(df))])
  df$Agency_ability <-
    rowMeansCustomMissing(df[, grepl("31|32|33", colnames(df))])
  df$Means_ends_effort <-
    rowMeansCustomMissing(df[, grepl("34|35|36", colnames(df))])
  df$Means_ends_ability <-
    rowMeansCustomMissing(df[, grepl("37|38|39", colnames(df))])
  df$Means_ends_luck <-
    rowMeansCustomMissing(df[, grepl("40|41|42", colnames(df))])
  df$Emotional_exhaustion <-
    rowMeansCustomMissing(df[, grepl("48|49|50", colnames(df))])
  df$Standards <-
    rowMeansCustomMissing(df[, grepl("51|52|53|54", colnames(df))])
  df$Discrepancy <-
    rowMeansCustomMissing(df[, grepl("55|56|57|58", colnames(df))])
  df$Others_expectations <-
    rowMeansCustomMissing(df[, grepl("59|60|61", colnames(df))])
  df$New_pursuit <-
    rowMeansCustomMissing(df[, grepl("62|65|[^MW]70", colnames(df))])
  df$Seeking_rewards <-
    rowMeansCustomMissing(df[, grepl("63|67|68|69", colnames(df))])
  df$Emotional_exposure <-
    rowMeansCustomMissing(df[, grepl("64|66", colnames(df))])
  return(df)
}


#' Generate summary for MS questionnaire
#'
#' NB the script sent seemed to have an error in the reverse coding of the MX_fixed items
#' I have assumed that those items should have been fully reversed as the comments seemed to support this idea
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form MS data
#'
#' @return wide form of MS data with summary vars
#'
#' @export
deriveBnuMS <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  # recode down by 1
  recodeVariables <- names(df)[grepl('MS', names(df))]
  df<-recodeVariables(df, recodeVariables, fun= function(x) {x-1})

  #Summary
  df$T1_MS_Growth <-
    rowSumsCustomMissing(df[, grepl("02|03|05|06|09|10|13|15|18|19", colnames(df))])
  df$T1_MS_Fixed <-
    rowSumsCustomMissing(df[, grepl("01|04|07|08|11|12|14|16|17|20", colnames(df))])
  df$T1_MS_4 <- ifelse(df$T1_MS_Growth + 30 - df$T1_MS_Fixed < 21, 1,
                       ifelse(df$T1_MS_Growth + 30 - df$T1_MS_Fixed < 34, 2,
                              ifelse(df$T1_MS_Growth + 30 - df$T1_MS_Fixed < 45, 3, 4)))
  df$T1_MS_2 <- ifelse(df$T1_MS_Growth + 30 - df$T1_MS_Fixed < 34, 1,2)

  return(df)
}

#' Generate summary for BG questionnaire
#'
#' All this does is to calculate AGE variables from the DOBs provided
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form BG data
#'
#' @return wide form of BG data with summary vars
#'
#' @export
deriveBnuBG <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)

  #Compute Age ( in years ) for the child based on the DOB and Processed Timestamp
  if("T1_BG_PC3" %in% names(df)) {
    df$Child_Age_Days<-floor(difftime(df$Processed.Timestamp, df$T1_BG_PC3, "days"))
  }
  if("T1_BG_C2" %in% names(df)) {
    df$Child_Age_Days<-floor(difftime(df$Processed.Timestamp,df$T1_BG_C2,  "days"))
  }

  return(df)
}

#' Generate summary for SPSRQ questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form SPSRQ data
#'
#' @return wide form of SPSRQ data with summary vars
#'
#' @export
deriveBnuSPSRQ <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summary
  df$Sensitivity_to_Punishment <-
    rowSumsCustomMissing(df[, grepl("02|05|07|09|12|15|17|18|19|21|23|26|29|31|33", colnames(df))])
  df$Impulsivity_Fun_Seeking <-
    rowSumsCustomMissing(df[, grepl("11|14|20|22|24|25|32", colnames(df))])
  df$Reward_Responsivity <-
    rowSumsCustomMissing(df[, grepl("01|03|04|06|08|10|13", colnames(df))])
  df$Drive_Scale <-
    rowSumsCustomMissing(df[, grepl("16|27|28|30", colnames(df))])
  df$Reward_Sensitivity <-
    rowSumsCustomMissing(df[, grepl("Impulsivity_Fun_Seeking|Reward_Responsivity|Drive_Scale", colnames(df))])
  df$Punishment_Sensitivity <- df$Sensitivity_to_Punishment
  return(df)
}


