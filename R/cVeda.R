# Additional functions for the cVEDA study
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


#' Generate summary for ACE-IQ questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' recodes religion language and caste as required for cVEDA
#'
#' @param df data frame containing long form ACE-IQ data
#'
#' @return wide form of ACE-IQ data with summary vars
#'
#' @export
deriveCvedaACEIQ <- function(df) {

    df <- rotateQuestionnaire(df)

    # recode open language religion and caste info
    if ("ACEIQ_C4a" %in% names(df)) {
        languageReligionCaste <- df[, grepl("ACEIQ_C4", names(df))]
        names(languageReligionCaste) <- gsub("ACEIQ_C4a", "language", names(languageReligionCaste))
        names(languageReligionCaste) <- gsub("ACEIQ_C4b", "religion", names(languageReligionCaste))
        names(languageReligionCaste) <- gsub("ACEIQ_C4c", "caste", names(languageReligionCaste))
        languageReligionCaste <- categoriseCvedaReligionLanguageCaste(languageReligionCaste)
        df$ACEIQ_C4a <- languageReligionCaste$language
        df$ACEIQ_C4a_specify <- languageReligionCaste$language_specify
        df$ACEIQ_C4b <- languageReligionCaste$religion
        df$ACEIQ_C4b_specify <- languageReligionCaste$religion_specify
        df$ACEIQ_C4c <- languageReligionCaste$caste
        df$ACEIQ_C4c_specify <- languageReligionCaste$caste_specify
    }

    # Summary

  df$Adversity.Binary <-
    ifelse(df$ACEIQ_A1>0|df$ACEIQ_A2>0, 1,0) +
    ifelse(df$ACEIQ_A3>0|df$ACEIQ_A4>0, 1,0) +
    ifelse(df$ACEIQ_A5>0|df$ACEIQ_A6>0|df$ACEIQ_A7>0|df$ACEIQ_A8>0, 1,0) +
    ifelse(df$ACEIQ_F6>0|df$ACEIQ_F7>0|df$ACEIQ_F8>0, 1,0) +
    ifelse(df$ACEIQ_F1>0, 1,0) +
    ifelse(df$ACEIQ_F2>0, 1,0) +
    ifelse(df$ACEIQ_F3>0, 1,0) +
    ifelse(df$ACEIQ_F4>0|df$ACEIQ_F5>0, 1,0) +
    ifelse(df$ACEIQ_P1==0|df$ACEIQ_P2==0, 1,0) +
    ifelse(df$ACEIQ_P3>0|df$ACEIQ_P4>0|df$ACEIQ_P5>0, 1,0) +
    ifelse(df$ACEIQ_V1>0, 1,0) +
    ifelse(df$ACEIQ_V4>0|df$ACEIQ_V5>0|df$ACEIQ_V6>0, 1,0) +
    ifelse(df$ACEIQ_V7>0|df$ACEIQ_V8>0|df$ACEIQ_V9>0|df$ACEIQ_V10>0, 1,0)

  df$Adversity.Frequency <-
    ifelse(df$ACEIQ_A1==3|df$ACEIQ_A2==3, 1,0) +
    ifelse(df$ACEIQ_A3==3|df$ACEIQ_A4==3, 1,0) +
    ifelse(df$ACEIQ_A5>0|df$ACEIQ_A6>0|df$ACEIQ_A7>0|df$ACEIQ_A8>0, 1,0) +
    ifelse(df$ACEIQ_F6==3|df$ACEIQ_F7>1|df$ACEIQ_F8>1, 1,0) +
    ifelse(df$ACEIQ_F1>0, 1,0) +
    ifelse(df$ACEIQ_F2>0, 1,0) +
    ifelse(df$ACEIQ_F3>0, 1,0) +
    ifelse(df$ACEIQ_F4>0|df$ACEIQ_F5>0, 1,0) +
    ifelse(df$ACEIQ_P1==0|df$ACEIQ_P2==0|df$ACEIQ_P1==1|df$ACEIQ_P2==1, 1,0) +
    ifelse(df$ACEIQ_P3==3|df$ACEIQ_P4==3|df$ACEIQ_P5==3, 1,0) +
    ifelse(df$ACEIQ_V1==3, 1,0) +
    ifelse(df$ACEIQ_V4==3|df$ACEIQ_V5==3|df$ACEIQ_V6==3, 1,0) +
    ifelse(df$ACEIQ_V7>0|df$ACEIQ_V8>0|df$ACEIQ_V9>0|df$ACEIQ_V10>0, 1,0)

  df$Addendum <-
    rowSumsCustomMissing(df[, grepl("ACEIQ_Ad", colnames(df))])

  df$CRIES.Abuse <-
    rowSumsCustomMissing(df[, grepl("CRIES_5", colnames(df))])

  df$CRIES.Bullying <-
    rowSumsCustomMissing(df[, grepl("CRIES_6", colnames(df))])

  df$CRIES.CollectiveViolence <-
    rowSumsCustomMissing(df[, grepl("CRIES_8", colnames(df))])

  df$CRIES.CommunityViolence <-
    rowSumsCustomMissing(df[, grepl("CRIES_7", colnames(df))])

  df$CRIES.Family <-
    rowSumsCustomMissing(df[, grepl("CRIES_4", colnames(df))])

  df$CRIES.Neglect <-
    rowSumsCustomMissing(df[, grepl("CRIES_3", colnames(df))])

  return(fixNumericVariables(df))
}



#' Generate summary for SDIM questionnaire
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' There are no specified summaries for this questionnaire - but the language religion and caste must be recoded
#'
#' @param df data frame containing long form SDIM data
#'
#' @return wide form of SDIM data with language religion and caste recoded
#'
#' @export
deriveCvedaSDIM <- function(df) {

    df <- rotateQuestionnaire(df)

    # recode open language religion and caste info
    if ("SDI_03" %in% names(df)) {
        languageReligionCaste <- df[, grepl("SDI_03|SDI_04|SDI_05", names(df))]
        names(languageReligionCaste) <- gsub("SDI_03", "language", names(languageReligionCaste))
        names(languageReligionCaste) <- gsub("SDI_05", "religion", names(languageReligionCaste))
        names(languageReligionCaste) <- gsub("SDI_04", "caste", names(languageReligionCaste))
        languageReligionCaste <- categoriseCvedaReligionLanguageCaste(languageReligionCaste)
        df$SDI_03 <- languageReligionCaste$language
        df$SDI_03_specify <- languageReligionCaste$language_specify
        df$SDI_05 <- languageReligionCaste$religion
        df$SDI_05_specify <- languageReligionCaste$religion_specify
        df$SDI_04 <- languageReligionCaste$caste
        df$SDI_04_specify <- languageReligionCaste$caste_specify
    }
    return(fixNumericVariables(df))
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
deriveCvedaPDS <- function(df) {
    # Rotate
    df <- rotateQuestionnaire(df)

    df$PDS_sum <- rowSums(stripCustomMissings(df[, grepl("02|04|05|06", colnames(df))]),
        na.rm = TRUE)

    df$PDS_stage[df$PDS_gender == "M" & df$PDS_sum >= 12] <- 5
    df$PDS_stage[df$PDS_gender == "M" & df$PDS_sum >= 9 & df$PDS_sum <= 11] <- 4
    df$PDS_stage[df$PDS_gender == "M" & df$PDS_sum >= 6 & df$PDS_sum <= 8] <- 3
    df$PDS_stage[df$PDS_gender == "M" & df$PDS_sum >= 4 & df$PDS_sum <= 5] <- 2
    df$PDS_stage[df$PDS_gender == "M" & df$PDS_sum < 4] <- 1

    df$PDS_stage[df$PDS_gender == "F" & df$PDS_07 == 3 & df$PDS_sum >= 8] <- 5
    df$PDS_stage[df$PDS_gender == "F" & df$PDS_07 == 3 & df$PDS_sum < 8] <- 4
    df$PDS_stage[df$PDS_gender == "F" & df$PDS_07 < 3 & df$PDS_sum > 3] <- 3
    df$PDS_stage[df$PDS_gender == "F" & df$PDS_07 != 3 & df$PDS_sum == 3] <- 2
    df$PDS_stage[df$PDS_gender == "F" & df$PDS_sum < 3] <- 1
    return(df)
}


#' Generate summary for cVeda's Anthropometry task - including homogenising units
#'
#' NB This does not select the appropriate attempt - this should be done by the calling function
#'
#' @param df data frame containing long form cVeda Antropomtery data
#'
#' @return wide form of Antropomtery data with summary vars and hand entered artefacts removed
#'
#' @export
deriveCvedaAnthropometry <- function(df) {
    # deal with hand entered units such as inch / cm .. and - have occasionally been used
    # as decimal delimiters might be worth checking the - was meant to be a decimal - it
    # makes no sense as a minus?  was 110001840790-C3 for Weight - certainly didnt mean
    # pounds and ounces
    df$Trial.result[grepl("\\.\\.|-", df$Trial.result)] <- gsub("\\.\\.|-", ".", df$Trial.result[grepl("\\.\\.|-",
        df$Trial.result)])

    # cm and KG are the right units - just strip them com has been entered once - assume
    # it's meant to be cm
    df$Trial.result[grepl("co?m|kg", df$Trial.result, ignore.case = TRUE)] <- gsub("[ kgoscm]*",
        "", df$Trial.result[grepl("co?m|kg", df$Trial.result, ignore.case = TRUE)], ignore.case = TRUE)

    # inches however need to be multiplied by 2.54 to yield CM inch is often spelt without
    # the c..?
    df$Trial.result[grepl("inc?h", df$Trial.result, ignore.case = TRUE)] <- as.numeric(gsub("[ inches]*",
        "", df$Trial.result[grepl("inc?h", df$Trial.result, ignore.case = TRUE)], ignore.case = TRUE)) *
        2.54

    # This leaves 3 non numeric values 35.7.  52.089.6 93.0157.7 Assuming that the second
    # decimal is simply erroneous as it leaves sensible values ( to a silly degree of
    # precision )
    df$Trial.result[grepl("(\\.[^.]*)\\.([^.]*)", df$Trial.result)] <- sub("(\\.[^.]*)\\.([^.]*)",
        "\\1\\2", df$Trial.result[grepl("(\\.[^.]*)\\.([^.]*)", df$Trial.result)])

    # Raise a warning if there are still some non-numeric values
    if (length(df$Trial.result[is.na(suppressWarnings(as.numeric(df$Trial.result))) & df$Trial.result !=
        "skip_back"]) > 0) {
        warning(paste("still finding non-numeric items in Anthro", df$Trial.result[is.na(suppressWarnings(as.numeric(df$Trial.result))) &
            df$Trial.result != "skip_back"]))
    }

    # Rotate
    df <- rotateQuestionnaire(df)
    return(df)
}

#' Apply Cveda custom missing / refused / not known coding values.
#' This should be run for every cVeda questionnaire prior to standard derivation
#' It is safe to run on all tasks also - no changes will be made
#'
#' This function defines a list customMissingValues in the parent scope
#' as well as a defaultUnadmininisteredValue (-666 for cVEDA)
#' which will be applied to all variables not seen by the Ppt
#'
#'
#' @param df data frame containing long form cVeda Data of any task
#'
#' @return long form with trial results recoded where required
#'
#' @export
applyCvedaCustomMissings <- function(df) {

    # remove all 'MaxVolume' Trials - Not relevant for cVEDA
    df <- df[df$Trial != "MaxVolume", ]

    # Recode Categorical changed to AllThatApply questions into allThatApply format
    df$Trial[df$Block %in% c("EEQ_04", "EEQ_11", "EEQ_21", "EEQ_22") & df$Trial %in% c("EEQ_04",
        "EEQ_11", "EEQ_21", "EEQ_22") & df$Trial.result != "refuse"] <- paste0(df$Trial[df$Block %in%
        c("EEQ_04", "EEQ_11", "EEQ_21", "EEQ_22") & df$Trial %in% c("EEQ_04", "EEQ_11",
        "EEQ_21", "EEQ_22") & df$Trial.result != "refuse"], "_", df$Trial.result[df$Block %in%
        c("EEQ_04", "EEQ_11", "EEQ_21", "EEQ_22") & df$Trial %in% c("EEQ_04", "EEQ_11",
        "EEQ_21", "EEQ_22") & df$Trial.result != "refuse"])

    df$Trial.result[df$Block %in% c("EEQ_04", "EEQ_11", "EEQ_21", "EEQ_22") & df$Trial.result >
        1 & df$Trial.result != "refuse"] <- 1

    # recode refusals on multiple choice questions to be refuse for ALL response options
    mcCols <- c("EEQ_04", "EEQ_11", "EEQ_21", "EEQ_22", "SDI_10", "SDI_29", "SDI_31", "EEQ_01",
        "EEQ_36", "EEQ_52", "EEQ_53", "EEQ_57", "SCAMP_P_q5", "SCAMP_P_q10", "SCAMP_S_q29",
        "SCAMP_S_q2")
    for (mcCol in mcCols) {
        df$Trial.result[df$Block == mcCol & df$User.code %in% df$User.code[df$Block ==
            mcCol & df$Trial.result == "refuse"] & df$Iteration %in% df$Iteration[df$Block ==
            mcCol & df$Trial.result == "refuse"]] <- "refuse"
    }

    # finally remove all allThatApply base question rows
    df <- df[!df$Trial %in% mcCols, ]

    # Instrument Specific
    df$Trial.result[grepl("SCAMP_S", df$Trial) & df$Trial.result == "refuse"] <- -777
    df$Trial.result[df$Trial == "SCAMP_S_q19" & df$Trial.result == "5"] <- -777

    df$Trial.result[grepl("SCAMP_P_q6", df$Trial) & df$Trial.result == "4"] <- -999
    df$Trial.result[grepl("SCAMP_P_q9|SCAMP_P_q10", df$Trial) & df$Trial.result == "refuse"] <- -999

    df$Trial.result[grepl("ACEIQ|IFVCS|PDS", df$Trial) & df$Trial.result == "R"] <- -888

    df$Trial.result[grepl("SCQ", df$Trial) & df$Trial.result == "5"] <- -888

    df$Trial.result[grepl("SDI_06", df$Trial) & df$Trial.result == "5"] <- -777
    df$Trial.result[grepl("SDI_19|SDI_20", df$Trial) & df$Trial.result == "10"] <- -777
    df$Trial.result[grepl("SDI_21|SDI_22|SDI_23", df$Trial) & df$Trial.result == "refuse"] <- -777
    df$Trial.result[grepl("SDI_34|SDI_41", df$Trial) & df$Trial.result == "9"] <- -999

    df$Trial.result[grepl("fhq", df$Trial) & df$Trial.result == "RF"] <- -888

    df$Trial.result[grepl("PHI", df$Trial) & df$Trial.result == "-7"] <- -888
    df$Trial.result[grepl("PHI", df$Trial) & df$Trial.result == "-8"] <- -999

    df$Trial.result[grepl("EEQ_06|EEQ_07", df$Trial) & df$Trial.result == "3"] <- -999
    df$Trial.result[grepl("EEQ_11", df$Trial) & df$Trial.result == "8"] <- -999
    df$Trial.result[grepl("EEQ_13|EEQ_14", df$Trial) & df$Trial.result == "4"] <- -999
    df$Trial.result[grepl("EEQ_37_EEQ_38", df$Trial) & df$Trial.result == "99"] <- -777
    df$Trial.result[grepl("EEQ_58", df$Trial) & df$Trial.result == "refuse"] <- -777

    # General
    df$Trial.result[df$Trial.result == "NK" | df$Trial.result == "DK"] <- -999

    df$Trial.result[df$Trial.result == "NA" | is.na(df$Trial.result)] <- -777

    df$Trial.result[df$Trial.result == "refuse" | df$Trial.result == "NR"] <- -888

    # define custom missing codes in the parent scope for use by other functions
    customMissingValues <<- c(-666, -777, -888, -999)
    customMissingValueLabels <<- c("not administered or revision in data collection procedure",
        "not applicable", "refused", "don't know")
    defaultUnadministeredValue <<- -666
    return(df)
}

#' Convert the opentext Language Religion Caste variables found in SDIM and ACEIQ
#' to the categorical format used in later versions
#'
#' @param df with the following variables:
#' language language responses
#' religion religion responses
#' caste caste responses
#' religion_specify language Other Specify Response
#' language_specify religion Other Specify Response
#' caste_specify caste Other Specify Responses
#'
#' @return df with categories replaced and uncategorised resposnes shifted to other specify
#'
#' @export
categoriseCvedaReligionLanguageCaste <- function(df) {
    # First move all hand entered 'other_specify' responses into the main columns so they
    # are filtered like the original hand entered It seems many people use other for things
    # like General even though there is a categorical button for it...
    df$language[df$language == 14 & df$language_specify != ""] <- df$language_specify[df$language ==
        14 & df$language_specify != ""]
    df$religion[df$religion == 8 & df$religion_specify != ""] <- df$religion_specify[df$religion ==
        8 & df$religion_specify != ""]
    df$caste[df$caste == "OTH" & df$caste_specify != ""] <- df$caste_specify[df$caste ==
        "OTH" & df$caste_specify != ""]

    # to recode lad (mix of kannada and hindi) as 5 ( as requested ) Kannada recoded first
    df$language[agrep("kannada", df$language, ignore.case = TRUE)] <- 5
    df$language[agrep("hindi", df$language, ignore.case = TRUE)] <- 1
    df$language[agrep("punjab", df$language, ignore.case = TRUE)] <- 2
    df$language[agrep("bengal", df$language, ignore.case = TRUE)] <- 3
    df$language[agrep("manipur", df$language, ignore.case = TRUE)] <- 4
    df$language[agrep("tamil", df$language, ignore.case = TRUE)] <- 6
    df$language[agrep("telugu", df$language, ignore.case = TRUE)] <- 7
    df$language[grepl("Tegala|Thadou|THELAGU", df$language, ignore.case = TRUE)] <- 7
    df$language[agrep("malyal", df$language, ignore.case = TRUE)] <- 8
    df$language[agrep("marathi", df$language, ignore.case = TRUE)] <- 9
    df$language[agrep("urdu", df$language, ignore.case = TRUE)] <- 10
    df$language[agrep("gujarathi", df$language, ignore.case = TRUE)] <- 11
    df$language[agrep("assam", df$language, ignore.case = TRUE)] <- 12
    df$language[agrep("ORIYA", df$language, ignore.case = TRUE)] <- 13
    df$language[grepl("^nr$", df$language, ignore.case = TRUE)] <- -888
    df$language[grepl("^nk$", df$language, ignore.case = TRUE)] <- -999

    df$religion[agrep("hindu", df$religion, ignore.case = TRUE)] <- 1
    df$religion[agrep("gowda", df$religion, ignore.case = TRUE)] <- 1
    df$religion[grepl("Kshatriya|LINGAYATHA|Madiwala|Manipuri|Marwadi|Padmashali", df$religion,
        ignore.case = TRUE)] <- 1
    df$religion[grepl("muslim|islam", df$religion, ignore.case = TRUE)] <- 2
    df$religion[grepl("sikh|suni|sunni|sihk", df$religion, ignore.case = TRUE)] <- 3
    # Should Protestant and RC not go in here too?
    df$religion[agrep("christ", df$religion, ignore.case = TRUE)] <- 4
    df$religion[grepl("CHTESTAIN|Chirst|Chirstan|CHRSITIAN", df$religion)] <- 4
    df$religion[agrep("jain", df$religion, ignore.case = TRUE)] <- 6
    df$religion[agrep("buddh", df$religion, ignore.case = TRUE)] <- 7
    df$religion[grepl("^nr$", df$religion, ignore.case = TRUE)] <- -888
    df$religion[grepl("^nk$", df$religion, ignore.case = TRUE)] <- -999

    df$caste[agrep("general", df$caste, ignore.case = TRUE)] <- "GEN"
    df$caste[trimws(tolower(df$caste)) %in% tolower(c("0KKALIGA", "awasthi", "BARBER",
        "Bengali", "BHAVASAR KSHATRIYA", "Bhovi", "Bhramin", "BRAHMIN", "BRAHMINS", "bramhins",
        "CATEGORY 1", "category1", "Choudary", "Devaga", "devanga", "devangas", "DHANOOK",
        "gen", "GENEARL", "genera", "general", "general merit", "generel", "GENREAL", "genrel",
        "GM", "Goudas", "Gouder", "Goundar", "Gounder", "gowd", "gowda", "GOWDA-GENERAL",
        "gowdas", "Guptas", "hind", "hindu", "islam", "JAIN", "JAINS", "JAMMAT", "kashyap",
        "KHANGAR", "Kotegar", "KSATRIYA", "Kshatriya", "Kshatriya sakulshali", "kunchitga",
        "KURUBA", "kurubas", "kurubga", "MARATHA", "marathi", "marathis", "marawadi", "Marawdi",
        "marthoma", "Marwadi", "MBC", "Meitei", "missionaries", "MUDHLIYAR", "Mudliars",
        "Musilam", "muslim", "Nadiu", "NAIDU", "nair", "namadhari", "NAMADHARI GOWDA",
        "NAMADHARI NGOWDA", "nayak", "nayaka", "Padmashali", "Padmashaliyar", "Pantosh",
        "parrier", "Pentacost", "PENTECOST", "poojarru", "prostestant", "PROTESTANT", "Protestant christian",
        "PROTESTANTS", "Punjabi", "Rajakodam Agamudaliar", "Rajput", "Rajputs", "RAJU KSHATRIYA",
        "RC", "REDDY", "Shatriya", "SHATRIYAS", "SHETTY", "sikh", "Sikh Punjabi", "Sindhi",
        "SRI VAISHNAVA", "Vahnikulam Kshatriya", "VAISHY", "vaishya", "vanikula kshatriya",
        "Vanikula Shatrias", "VEERASHAIVA", "Vishkarma", "Vishwa karma", "Vishwakarma",
        "VOKKALIGA", "vokkaliga gowda", "vokkaliga gowdas", "vyasha", "Vysyas", "Wodeyar",
        "YADAV", "Yadava", "yadavas"))] <- "GEN"
    df$caste[agrep("other back", df$caste, ignore.case = TRUE)] <- "OBC"
    df$caste[trimws(tolower(df$caste)) %in% tolower(c("0ther bakward classes", "0BC", "backward class",
        "bc", "BCD", "bcma", "obc", "oc"))] <- "OBC"
    df$caste[trimws(tolower(df$caste)) %in% tolower(c("sc", "SCC", "schedule caste", "SCHEDULED",
        "scheduled caste", "secheduled caste"))] <- "SC"
    df$caste[trimws(tolower(df$caste)) %in% tolower(c("nayak ST", "nayakas - ST", "Nayakas ST",
        "SCHEDULE TRIBE", "Scheduled tribe", "ST"))] <- "ST"
    df$caste[grepl("don'?t know|^dk$|^nk|not known", df$caste, ignore.case = TRUE)] <- -999
    df$caste[grepl("refused|^nr$", df$caste, ignore.case = TRUE)] <- -888

    # Set the 'other' coding for the early responses that cannot be mapped to categorical
    df$language_specify[is.na(suppressWarnings(as.numeric(df$language)))] <- df$language[is.na(suppressWarnings(as.numeric(df$language)))]
    df$language[is.na(suppressWarnings(as.numeric(df$language)))] <- 14
    df$religion_specify[is.na(suppressWarnings(as.numeric(df$religion)))] <- df$religion[is.na(suppressWarnings(as.numeric(df$religion)))]
    df$religion[is.na(suppressWarnings(as.numeric(df$religion)))] <- 8
    df$caste_specify[!(df$caste %in% c("OTH", "GEN", "SC", "ST", "OBC", "-999", "-888",
        "-666", "-777"))] <- df$caste[!(df$caste %in% c("OTH", "GEN", "SC", "ST", "OBC",
        "-999", "-888", "-666", "-777"))]
    df$caste[!(df$caste %in% c("OTH", "GEN", "SC", "ST", "OBC", "-999", "-888", "-666",
        "-777"))] <- "OTH"

    return(df)
}
