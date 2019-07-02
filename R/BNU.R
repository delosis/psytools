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



#' Generate summary for PDS questionnaire
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
  
  #Summary
  df$PDS_sum <-
    rowSumsCustomMissing(df[, grepl("2|4|5|6", colnames(df))])
  
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 12]<-5
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 9 & df$PDS_sum <= 11]<-4
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 6 & df$PDS_sum <= 8]<-3
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum >= 4 & df$PDS_sum <= 5]<-2
  df$PDS_stage[df[,grepl('gender',names(df))]=='1' & df$PDS_sum < 4]<-1
  
  df$PDS_stage[df[,grepl('gender',names(df))] =='2' & df[,grepl('7',names(df))] ==3 & df$PDS_sum>=8]<-5
  df$PDS_stage[df[,grepl('gender',names(df))] =='2' & df[,grepl('7',names(df))] ==3 & df$PDS_sum < 8]<-4
  df$PDS_stage[df[,grepl('gender',names(df))] =='2' & df[,grepl('7',names(df))] < 3 & df$PDS_sum > 3]<-3
  df$PDS_stage[df[,grepl('gender',names(df))]=='2' & df[,grepl('7',names(df))] !=3 & df$PDS_sum == 3]<-2
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
  reverseVariables <- c('38')
  df<-recodeVariables(df, reverseVariables, fun= function(x) {5-x})
  
  #Summary
  df$Faith_of_school <-
    rowSumsCustomMissing(df[, grepl("37|38R|39|40", colnames(df))])
  
  return(df)
}

#' Generate summary for LS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form LS data
#'
#' @return wide form of LS data with summary vars
#'
#' @export
deriveBnuLS <- function(df) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  # reverse code
  reverseVariables <- c('01','03','05','07','10','15')
  df<-recodeVariables(df, reverseVariables, fun= function(x) {5-x})
  
  #Summary
  df$Loneliness_sum <-
    rowSumsCustomMissing(df[, grepl("LS", colnames(df))])
  
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

#' Generate summary for BISBAS questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form BISBAS data
#'
#' @return wide form of BISBAS data with summary vars
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

#' Generate summary for basic questionnaire with summary total
#' @param df data frame containing long form data
#' @param Qname Name contained in all variables to be summed
#' @return wide form of data with sum
#'
#' @export
deriveBnuSimpleSum <- function(df, Qname) {
  #Rotate
  df <- rotateQuestionnaire(df)
  
  #Summary
  df[,paste0(Qname,'_sum')]<-
         rowSumsCustomMissing(df[, grepl(Qname, colnames(df))])
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
  
  #Summary
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
  df$Temperament <-
    rowMeansCustomMissing(df[, grepl("62|63|64|65|66|67|68|69|[^MW]70", colnames(df))])
  df$Fear_of_fauilure <-
    rowMeansCustomMissing(df[, grepl("01|02|03", colnames(df))])
  df$Academic_withdrawal <-
    rowMeansCustomMissing(df[, grepl("04|05|06", colnames(df))])
  df$Self_esteem <-
    rowMeansCustomMissing(df[, grepl("07|08|09|10|11", colnames(df))])
  df$School_value <-
    rowMeansCustomMissing(df[, grepl("12|43|44|45|46|47", colnames(df))])
  df$Emotional_exhaustion <-
    rowMeansCustomMissing(df[, grepl("48|49|50", colnames(df))])
  df$Standards <-
    rowMeansCustomMissing(df[, grepl("51|52|53|54", colnames(df))])
  df$Discrepancy <-
    rowMeansCustomMissing(df[, grepl("55|56|57|58", colnames(df))])
  df$Discrepancy <-
    Others_expectations(df[, grepl("59|60|61", colnames(df))])
  
  return(df)
}