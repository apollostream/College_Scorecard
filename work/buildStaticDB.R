#' ---
#' title: "Build DataSpec for BestCollegeApp"
#' author: "Michael L. Thompson"
#' date: "March 13, 2016"
#' output:
#'   html_document:
#'     toc: yes
#'   pdf_document:
#'     includes:
#'       in_header: header.tex
#'     toc: yes
#' always_allow_html: yes
#' ---
#' \newpage
#' 
#' # Introduction
#' 
#' This script simply generates the `DataSpec` object with the transformed
#' College Scorecard dataset and the utility function.
#' 
#' This is an excerpt script derived from "Which College is Best for You? Part
#' 3". At this time, this script is the same as the script "Which College is
#' Best for You?_Part 2". My intention is to add a variable to account for
#' cost/affordability based on in-state/out-of-state tuition and financial aid.
#' 
#' As before...
#' This is a followup to my first script ["Which College is Best for You?_"](https://www.kaggle.com/apollostar/d/kaggle/college-scorecard/which-college-is-best-for-you).
#' 
#' Ben Hamner suggested in his comments to that first script that I write a
#' script allowing anyone to specify his or her student profile right at the
#' top. In this way the script might be more appealing for other folks to fork
#' and play with it themselves.  Good advice!
#' 
#' So here it is.
#' 
#' One thing though,... Using RStudio on this R Markdown file, you can click on
#' "Chunks-->Run All" to save the variables in the global environment. Then it
#' won't have to re-process the database every time you make a change to the
#' student profile and re-run it each time with "Chunks-->Run All" to generate a
#' new top schools list.
#' 
#' 
#' 
#' # Setup the Data & Model
#' The next chunk loads the college database and defines necessary data structures and functions to implement the model.
#' 
#+  libraries,echo=FALSE, message=FALSE, warning=FALSE
library('RSQLite')   # for SQLite manipulation of database
library('magrittr')  # for piping with infix %>%, %<>%, %T>% (for cool, sophistication effect)
library('tidyr')     # for reshaping data (e.g., 'gather')
library('dplyr')     # for data wrangling (e.g., 'filter', 'select', 'summarize', 'inner_join')
library('ggplot2')   # for best-in-class visualization
library('gridExtra') # for grid layout of ggplots
library('bnlearn')   # for learning BBN from datasets, and also provides function "discretize"
# library('leaflet')   # for mapping of the top schools for each student
# library('htmltools') # for assistance with leaflet
library('RColorBrewer') # for color palette management
#library('xtable')    # for rendering tables in HTML
library(knitr)       # for displaying tables that render both in HTML & PDF
library( PReMiuM )   # for clustering schools by netprice so as to impute missing values
# ```
#' 
#+  setupFunc, results='hide', echo=FALSE, message=FALSE, warning=FALSE,cache=TRUE,autodep=TRUE
#================================================================
# Function Setup: Loads the college database, processes into Bayes factors, and defines the data structures need for modeling.
Setup <- function(resetIt = FALSE, saveIt = FALSE,dbpath = "../input"){
  if(!resetIt && file.exists('DataSpec.RData')) {
    load('DataSpec.RData')
    return(DataSpec)
  }
  # Academic disciplines:
  discgrp <- data_frame(LABEL=c("Agriculture, Agriculture Operations, and Related Sciences",
                                "Natural Resources and Conservation",
                                "Architecture and Related Services",
                                "Area, Ethnic, Cultural, Gender, and Group Studies",
                                "Communication, Journalism, and Related Programs",
                                "Communications Technologies/Technicians and Support Services",
                                "Computer and Information Sciences and Support Services",
                                "Personal and Culinary Services",
                                "Education",
                                "Engineering",
                                "Engineering Technologies and Engineering-Related Fields",
                                "Foreign Languages, Literatures, and Linguistics",
                                "Family and Consumer Sciences/Human Sciences",
                                "Legal Professions and Studies",
                                "English Language and Literature/Letters",
                                "Liberal Arts and Sciences, General Studies and Humanities",
                                "Library Science",
                                "Biological and Biomedical Sciences",
                                "Mathematics and Statistics",
                                "Military Technologies and Applied Sciences",
                                "Multi/Interdisciplinary Studies",
                                "Parks, Recreation, Leisure, and Fitness Studies",
                                "Philosophy and Religious Studies",
                                "Theology and Religious Vocations",
                                "Physical Sciences",
                                "Science Technologies/Technicians","Psychology",
                                "Homeland Security, Law Enforcement, Firefighting and Related Protective Services",
                                "Public Administration and Social Service Professions","Social Sciences",
                                "Construction Trades",
                                "Mechanic and Repair Technologies/Technicians",
                                "Precision Production",
                                "Transportation and Materials Moving",
                                "Visual and Performing Arts",
                                "Health Professions and Related Programs",
                                "Business, Management, Marketing, and Related Support Services",
                                "History"),
                        discgrp=c(2,4,2,5,6,4,1,4,5,1,1,5,5,7,5,5,5,2,1,4,5,4,5,5,2,3,3,4,3,3,4,4,4,4,6,2,7,5),
                        VARIABLE.NAME=c("PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13",
                                        "PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26",
                                        "PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42",
                                        "PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51",
                                        "PCIP52","PCIP54"))
  
  discNames <- gsub('^([A-Z][-a-z]+)[, and/]*([A-Z][a-z]+).*','\\1\\2',discgrp$LABEL)
  discgrp %<>% tbl_df %>% mutate(discName = discNames) 
  
  # Get the college database: if not already exist, data wrangle & compute Bayes factors. 
  cat("Loading college database ...")
  #  if(!exists('studentBF') | !exists('student')){
  #     if(file.exists("studentBF.RData")){
  #       load("studentBF.RData")
  #     } 
  #     if(file.exists("student.RData")){
  #       load("student.RData")
  #     }
  #    if(!exists('studentBF') | !exists('student')){
  # If still don't have these objects 'student' & 'studentBF', build them from the database file.
  
  db <- dbConnect(dbDriver("SQLite"), paste(dbpath,"database.sqlite",sep = "/"))
  # This stops SQLite from writing temp files to disk, which use all the available space
  dbGetQuery(db, "PRAGMA temp_store=2;") 
  
  fieldNames <-
    c(
      'UGDS','UGDS_WHITE','UGDS_BLACK','UGDS_HISP','UGDS_ASIAN','UGDS_AIAN','UGDS_NHPI',
      'UGDS_2MOR','UGDS_NRA','UGDS_UNKN',"UG25abv","INC_PCT_LO","DEP_STAT_PCT_IND","DEP_INC_PCT_LO","IND_INC_PCT_LO",
      "DEP_INC_PCT_M1","DEP_INC_PCT_M2","DEP_INC_PCT_H1","IND_INC_PCT_M1","IND_INC_PCT_M2","IND_INC_PCT_H1","IND_INC_PCT_H2",
      "DEP_INC_PCT_H2","PAR_ED_PCT_1STGEN","pct_white","pct_black","pct_asian","pct_hispanic","pct_ba","pct_grad_prof",
      "pct_born_us","md_faminc","median_hh_inc","poverty_rate","unemp_rate","ln_median_hh_inc","pell_ever","female","fsend_1","fsend_2",
      "fsend_3","fsend_4","fsend_5","INC_PCT_LO","INC_PCT_M1","INC_PCT_M2","INC_PCT_H1","INC_PCT_H2","LATITUDE",
      "LONGITUDE","ZIP","STABBR","region","LOCALE","CCSIZSET","DISTANCEONLY","RELAFFIL","CURROPER","NPT4_PUB",
      "NPT4_PRIV","NPT41_PUB","NPT42_PUB","NPT43_PUB","NPT44_PUB","NPT45_PUB","NPT41_PRIV","NPT42_PRIV","NPT43_PRIV",
      "NPT44_PRIV","NPT45_PRIV","TUITIONFEE_IN","TUITIONFEE_OUT","PCTPELL","ADM_RATE","RET_FT4","SATVR25","SATVR75",
      "SATMT25","SATMT75","SATWR25","SATWR75","SATVRMID","SATMTMID","SATWRMID","ACTCM25","ACTCM75","ACTEN25","ACTEN75",
      "ACTMT25","ACTMT75","ACTWR25","ACTWR75","ACTCMMID","ACTENMID","ACTMTMID","ACTWRMID","SAT_AVG","SAT_AVG_ALL",
      "PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12",
      "PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30",
      "PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49",
      "PCIP50","PCIP51","PCIP52","PCIP54","C150_4_WHITE","C150_4_BLACK","C150_4_HISP","C150_4_ASIAN",'C150_4_AIAN','C150_4_NHPI',
      'C150_4_2MOR','C150_4_NRA','C150_4_UNKN',"ENRL_ORIG_YR2_RT",
      "LO_INC_ENRL_ORIG_YR2_RT","MD_INC_ENRL_ORIG_YR2_RT","HI_INC_ENRL_ORIG_YR2_RT","DEP_ENRL_ORIG_YR2_RT","IND_ENRL_ORIG_YR2_RT",
      "FEMALE_ENRL_ORIG_YR2_RT","MALE_ENRL_ORIG_YR2_RT","PELL_ENRL_ORIG_YR2_RT","NOPELL_ENRL_ORIG_YR2_RT","LOAN_ENRL_ORIG_YR2_RT",
      "NOLOAN_ENRL_ORIG_YR2_RT","FIRSTGEN_ENRL_ORIG_YR2_RT","NOT1STGEN_ENRL_ORIG_YR2_RT","ENRL_ORIG_YR3_RT","LO_INC_ENRL_ORIG_YR3_RT",
      "MD_INC_ENRL_ORIG_YR3_RT","HI_INC_ENRL_ORIG_YR3_RT","DEP_ENRL_ORIG_YR3_RT","IND_ENRL_ORIG_YR3_RT","FEMALE_ENRL_ORIG_YR3_RT",
      "MALE_ENRL_ORIG_YR3_RT","PELL_ENRL_ORIG_YR3_RT","NOPELL_ENRL_ORIG_YR3_RT","LOAN_ENRL_ORIG_YR3_RT","NOLOAN_ENRL_ORIG_YR3_RT",
      "FIRSTGEN_ENRL_ORIG_YR3_RT","NOT1STGEN_ENRL_ORIG_YR3_RT","ENRL_ORIG_YR4_RT","LO_INC_ENRL_ORIG_YR4_RT","MD_INC_ENRL_ORIG_YR4_RT",
      "HI_INC_ENRL_ORIG_YR4_RT","DEP_ENRL_ORIG_YR4_RT","IND_ENRL_ORIG_YR4_RT","FEMALE_ENRL_ORIG_YR4_RT","MALE_ENRL_ORIG_YR4_RT",
      "PELL_ENRL_ORIG_YR4_RT","NOPELL_ENRL_ORIG_YR4_RT","LOAN_ENRL_ORIG_YR4_RT","NOLOAN_ENRL_ORIG_YR4_RT","FIRSTGEN_ENRL_ORIG_YR4_RT",
      "NOT1STGEN_ENRL_ORIG_YR4_RT","ENRL_ORIG_YR6_RT","LO_INC_ENRL_ORIG_YR6_RT","MD_INC_ENRL_ORIG_YR6_RT","HI_INC_ENRL_ORIG_YR6_RT",
      "DEP_ENRL_ORIG_YR6_RT","IND_ENRL_ORIG_YR6_RT","FEMALE_ENRL_ORIG_YR6_RT","MALE_ENRL_ORIG_YR6_RT","PELL_ENRL_ORIG_YR6_RT",
      "NOPELL_ENRL_ORIG_YR6_RT","LOAN_ENRL_ORIG_YR6_RT","NOLOAN_ENRL_ORIG_YR6_RT","FIRSTGEN_ENRL_ORIG_YR6_RT","NOT1STGEN_ENRL_ORIG_YR6_RT",
      "ENRL_ORIG_YR8_RT","LO_INC_ENRL_ORIG_YR8_RT","MD_INC_ENRL_ORIG_YR8_RT","HI_INC_ENRL_ORIG_YR8_RT","DEP_ENRL_ORIG_YR8_RT",
      "IND_ENRL_ORIG_YR8_RT","FEMALE_ENRL_ORIG_YR8_RT","MALE_ENRL_ORIG_YR8_RT","PELL_ENRL_ORIG_YR8_RT","NOPELL_ENRL_ORIG_YR8_RT",
      "LOAN_ENRL_ORIG_YR8_RT","NOLOAN_ENRL_ORIG_YR8_RT","FIRSTGEN_ENRL_ORIG_YR8_RT","NOT1STGEN_ENRL_ORIG_YR8_RT","C150_4_POOLED_SUPP",
      "PCTFLOAN","DEBT_MDN","GRAD_DEBT_MDN","WDRAW_DEBT_MDN","LO_INC_DEBT_MDN","MD_INC_DEBT_MDN","HI_INC_DEBT_MDN","DEP_DEBT_MDN",
      "IND_DEBT_MDN","PELL_DEBT_MDN","NOPELL_DEBT_MDN","CUML_DEBT_N","CUML_DEBT_P90","CUML_DEBT_P75","CUML_DEBT_P25",
      "CUML_DEBT_P10","loan_ever","DEBT_MDN_SUPP","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","CDR2","CDR3","COMPL_RPY_1YR_RT",
      "NONCOM_RPY_1YR_RT","LO_INC_RPY_1YR_RT","MD_INC_RPY_1YR_RT","HI_INC_RPY_1YR_RT","DEP_RPY_1YR_RT","IND_RPY_1YR_RT",
      "PELL_RPY_1YR_RT","NOPELL_RPY_1YR_RT","FEMALE_RPY_1YR_RT","MALE_RPY_1YR_RT","FIRSTGEN_RPY_1YR_RT","NOTFIRSTGEN_RPY_1YR_RT",
      "RPY_3YR_RT","COMPL_RPY_3YR_RT","NONCOM_RPY_3YR_RT","LO_INC_RPY_3YR_RT","MD_INC_RPY_3YR_RT","HI_INC_RPY_3YR_RT","DEP_RPY_3YR_RT",
      "IND_RPY_3YR_RT","PELL_RPY_3YR_RT","NOPELL_RPY_3YR_RT","FEMALE_RPY_3YR_RT","MALE_RPY_3YR_RT","FIRSTGEN_RPY_3YR_RT",
      "NOTFIRSTGEN_RPY_3YR_RT","RPY_5YR_RT","COMPL_RPY_5YR_RT","NONCOM_RPY_5YR_RT","LO_INC_RPY_5YR_RT","MD_INC_RPY_5YR_RT",
      "HI_INC_RPY_5YR_RT","DEP_RPY_5YR_RT","IND_RPY_5YR_RT","PELL_RPY_5YR_RT","NOPELL_RPY_5YR_RT","FEMALE_RPY_5YR_RT",
      "MALE_RPY_5YR_RT","FIRSTGEN_RPY_5YR_RT","NOTFIRSTGEN_RPY_5YR_RT","RPY_7YR_RT","COMPL_RPY_7YR_RT","NONCOM_RPY_7YR_RT",
      "LO_INC_RPY_7YR_RT","MD_INC_RPY_7YR_RT","HI_INC_RPY_7YR_RT","DEP_RPY_7YR_RT","IND_RPY_7YR_RT","PELL_RPY_7YR_RT",
      "NOPELL_RPY_7YR_RT","FEMALE_RPY_7YR_RT","MALE_RPY_7YR_RT","FIRSTGEN_RPY_7YR_RT","NOTFIRSTGEN_RPY_7YR_RT","count_ed",
      "count_nwne_p6","count_wne_p6","mn_earn_wne_p6","md_earn_wne_p6","pct10_earn_wne_p6","pct25_earn_wne_p6",
      "pct75_earn_wne_p6","pct90_earn_wne_p6","sd_earn_wne_p6","gt_25k_p6","mn_earn_wne_inc1_p6","mn_earn_wne_inc2_p6",
      "mn_earn_wne_inc3_p6","veteran"
    )
  # This dropped name set is part of the base query string...
  fieldNames <- setdiff(fieldNames,c('CITY','LONGITUDE','LATITUDE','ZIP','region','LOCALE','CURROPER'))
  # drop the discipline fields; they're grabbed separately. 
  fieldNames <- grep('PCIP',fieldNames,value = TRUE,invert = TRUE) 
  
  # Upper case variables are from 2013 and lower case variables are from the
  # Treasury dataset of 2005, except 'region'. fromS10 <-
  # grep('ENRL_ORIG',fieldNames)
  fromS11 <- which(fieldNames %in% grep('^[A-Z]|region',fieldNames))
  fromS05 <- setdiff(seq_along(fieldNames),fromS11)
  # put a '_2005' suffix on the Treasury variables.
  dfNames <- ifelse(grepl('^[A-Z]|region',fieldNames),fieldNames,paste(fieldNames,"2005",sep="_")) 
  
  makeQuery <- function(year,fieldNames,dfNames) {
    paste("SELECT UNITID unitID,
          INSTNM College,
          INSTURL url,
          CONTROL CollegeType,
          PREDDEG degree,
          CURROPER currop,
          DISTANCEONLY distance,
          RELAFFIL relaffil,
          CITY city,
          LONGITUDE lon,
          LATITUDE  lat,
          ZIP zip,
          st_fips state,
          region region,
          LOCALE locale,
          CCBASIC ccbasic,
          CCUGPROF ccugprof,
          Year Year,",
          paste(fieldNames,' ',dfNames,sep="",collapse = ","),
          "FROM Scorecard 
          WHERE Year=",year)
  }
  
  queryString2005 <- makeQuery(2005,fieldNames,dfNames)
  queryString2013 <- makeQuery(2013,fieldNames,dfNames)
  
  
  #       discNames <- gsub('^([A-Z][-a-z]+)[, and/]*([A-Z][a-z]+).*','\\1\\2',discgrp$LABEL)
  #       discgrp %<>% tbl_df %>% mutate(discName = discNames)
  queryStringDscplns <- makeQuery(2013,discgrp$VARIABLE.NAME,discNames)
  
  # Retrieve the data for 2005 because that is the latest with Treasury data on student families.
  student2005 <- dbGetQuery(db,queryString2005)
  # Retrieve the data for 2013 because that's the latest data.
  student2013 <- dbGetQuery(db,queryString2013)
  disciplines2013 <- dbGetQuery(db,queryStringDscplns)
  
  #       cat("\n")
  #       disciplines2013 %>% 
  #         filter(grepl('Harvard|Cal.+Inst.+Tech|Mass.+Inst.+Tech|Princeton U|Northwestern U|Cornell U',College)) %>% 
  #         dplyr::select(College,Engineering) %>%
  #         mutate(EngineeringProb = 100*Engineering) %>%
  #         show
  
  
  # Disconnect the database.
  dbDisconnect(db)
  
  #*** 2016/06/11: Discovered that Princeton University has "0" for "ComputerInformation"!
  #disciplines2013 %>% filter(College == "Princeton University") %>% select(1:2,ComputerInformation) %>% print
  # Based on this web page (https://www.cs.princeton.edu/people/ugrad), I estimated ~450 computer science majors out of the 5234 undergraduates enrolled at Princeton.
  # And since the Computer Science dept. is in the Engineering school, I'll set "ComputerInformation" to 450/5234=8.6% and subtract
  # that same amount from "Engineering".
  csprop <- 0.086
  engg   <-  disciplines2013[ disciplines2013$College == "Princeton University", "Engineering" ][[ 1 ]]
  disciplines2013[ disciplines2013$College == "Princeton University", c( "ComputerInformation", "Engineering") ] <- c( csprop, engg - csprop )
  #*** 2016/07/30: Similarly for Ohio St. Univ.
  csprop <- 0.086 # Just guessing....
  engg   <-  disciplines2013[ disciplines2013$College == "Ohio State University-Main Campus", "Engineering" ][[ 1 ]]
  csprop <-  disciplines2013[ disciplines2013$College == "Ohio State University-Main Campus", "ComputerInformation" ][[ 1 ]] + engg/4 # Just guessing....
  disciplines2013[ disciplines2013$College == "Ohio State University-Main Campus", c( "ComputerInformation", "Engineering") ] <- c( csprop, engg*3/4 )
  
  #*** 2016/06/27: Discovered that Princeton University also has "0" for "MultiInterdisciplinary"! Same with University of Chicago.
  show( disciplines2013[ grepl( "(Harvard|Yale|Stanford|Princeton) University|Massachusetts Institute of Technology|Columbia University in the City of New York|University of Chicago", disciplines2013$College ), c("College","MultiInterdisciplinary") ] )
  # Set it to median of the elite Ivy League
  ivyMedian <- median( disciplines2013[ grepl( "(Harvard|Yale|Stanford) University|Massachusetts Institute of Technology|Columbia University in the City of New York", disciplines2013$College ), "MultiInterdisciplinary" ] )
  curSums   <- disciplines2013[ disciplines2013$College %in% c("Princeton University","University of Chicago"), setdiff( discNames, "MultiInterdisciplinary" ) ] %>% rowSums
  disciplines2013[ disciplines2013$College %in% c("Princeton University","University of Chicago"), setdiff( discNames, "MultiInterdisciplinary" ) ] %<>% divide_by( ivyMedian + curSums )
  disciplines2013[ disciplines2013$College %in% c("Princeton University","University of Chicago"), "MultiInterdisciplinary" ] <- ivyMedian / ( ivyMedian + curSums )
  show( disciplines2013[ grepl( "(Harvard|Yale|Stanford|Princeton) University|Massachusetts Institute of Technology|Columbia University in the City of New York|University of Chicago", disciplines2013$College ), c("College","MultiInterdisciplinary") ] )
  
  # Join the years together.
  student2013 <- student2013[!sapply(student2013,function(x) all(is.na(x)))] %>% tbl_df
  student2005 <- student2005[!sapply(student2005,function(x) all(is.na(x)))] %>% tbl_df
  # student <- student2013 %>% tbl_df %>% 
  #   dplyr::select(-contains('_2005')) %>% 
  #   inner_join(student2005 %>% dplyr::select(unitID,contains('_2005')), by = "unitID")
  student <- student2013 %>% 
    inner_join(student2005 %>% dplyr::select(unitID,contains('_2005')), by = "unitID")
  # Add the dominant disciplines
  discplns <- disciplines2013 %>% tbl_df %>%
    filter(unitID %in% student$unitID) %>%
    mutate(irow = seq_len(nrow(.))) %>%
    gather(key=Discipline,value=Proportion,one_of(discNames)) %>%
    group_by(irow) %>%
    arrange(desc(Proportion)) %>%
    summarize(
      unitID   = first(unitID),
      College  = first(College),
      domDisc1 = first(Discipline),
      pctDisc1 = 100*first(Proportion),
      domDisc2 = nth(Discipline,n=2),
      pctDisc2 = 100*nth(Proportion,n=2)
    ) %>%
    mutate(
      isSpecialty = pctDisc1 > 70,
      isTheological = grepl('Theolo|Relig',domDisc1) | grepl('Theolo|Relig',domDisc2)
    )
  
  # CALLED THE MAIN DATA TABLE `student` BECAUSE ORIGINALLY ONLY CONTAINED
  # `student` CATEGORY OF DATA FIELDS. NOW THIS LEGACY NAME AWKWARDLY CONFUSES
  # THIS DATA TABLE OF UNIVERSITIES/COLLEGES WITH THE OTHER DATA OBJECTS
  # CONTAINING SPECIFIC STUDENT PROPERTIES -- SEE LATER...
  
  # Now add the disciplines as grouped
  student %<>% 
    inner_join(discplns %>% dplyr::select(-College),by='unitID')
  
  usa.states.dc <- c("District of Columbia",state.name)
  # Filter down to just the colleges meeting the following criteria:
  student %<>% 
    filter(CollegeType != 'Private for-profit',
           currop      == 'Currently certified as operating',
           distance    == 'Not distance-education only',
           degree      == "Predominantly bachelor's-degree granting",
           as.character(region)      != 'U.S. Service Schools',
           !grepl('^Associate',ccbasic),
           state %in% usa.states.dc,
           !is.na(ccbasic),
           !is.na(UGDS),
           !is.na(pell_ever_2005),
           !isTheological,
           !is.na(mn_earn_wne_inc2_p6_2005))
  
  # Turn character columns into factors.
  student <- student %>% lapply(
    function(x) 
      if(is.character(x) | is.factor(x) | is.logical(x)) {
        if('PrivacySuppressed' %in% x) {
          tmp<-x;tmp[x=='PrivacySuppressed']<-NA;as.numeric(tmp)
        } else {
          factor(gsub("â€™","'",x))
        }
      } else as.numeric(x)) %>% 
    as.data.frame %>% tbl_df
  
  # add a religious affiliation indicator variable:
  student %<>% 
    mutate(relaffilFLAG=factor(ifelse(is.na(relaffil),
                                      FALSE,
                                      !grepl('^[Nn]ot ',as.character(relaffil)))))
  
  
  a<- student %>% dplyr::select(grep('^(SAT|ACT)',names(.))) %>%  apply(1,function(x) !all(is.na(x)))
  student %<>% mutate(stdzdtest=a)
  
  #show(dim(student))
  #show(summary(student))
  
  
  #       Drop all the Penn St. and U. of Pitt. schools except the main campus because
  #       same 2005 Treasury data entered in all rows for each, resp.
  
  stdttmp <- student %>% filter((College %in% c('Pennsylvania State University-Main Campus',
                                                'University of Pittsburgh-Pittsburgh Campus')) |
                                  !(College %in% grep('Pennsylvania State University|University of Pittsburgh',
                                                      College,value=TRUE)))
  #       show(setdiff(student$College,stdttmp$College))
  #       show(grep('Pennsylvania State University|University of Pittsburgh',stdttmp$College,value=TRUE))
  student <- stdttmp
  rm(stdttmp)
  student <- student[!sapply(student,function(x) length(which(is.na(x)))>1000)]
  # Drop schools without earnings data:
  #       student %>% filter(is.na(pct10_earn_wne_p6_2005) | pct10_earn_wne_p6_2005==0) %>% 
  #         dplyr::select(unitID,College,UGDS,matches('pct.+earn')) 
  
  ## IMPUTATION
  student %<>% 
    filter(!is.na(pct10_earn_wne_p6_2005) & pct10_earn_wne_p6_2005>0) %>%
    mutate( UG25abv = ifelse( is.na(UG25abv), 0, UG25abv ) ) #%>%
    # mutate_each( 
    #   funs(ifelse( is.na(.), median(.,na.rm=TRUE), . )), 
    #   NPT4_PUB ,NPT4_PRIV ,NPT41_PUB ,NPT42_PUB ,NPT43_PUB ,NPT44_PUB,
    #   DEP_INC_PCT_M1, DEP_INC_PCT_M2, DEP_INC_PCT_H1, IND_INC_PCT_M1, IND_INC_PCT_M2, IND_INC_PCT_H1 
    # )
  
  ### Ethnicity:
  ethnicity <- student %>% 
    dplyr::select(unitID,College,contains("UGDS")) %>%
    mutate( UGDS_OTHER = UGDS_AIAN + UGDS_NHPI + UGDS_2MOR + UGDS_UNKN ) %>%
    mutate( probSchool = UGDS/sum(UGDS),
            totprob    = rowSums(as.matrix(.[4:ncol(.)])))
  #ethnicity %>% print(n=20)
  
  # This computes P(E | not(H)) the conditional probability of evidence given the
  # hypothesis is FALSE using inputs of cpH = P(E | H) probability of evidence
  # given the hypothesis is TRUE and pH = P(H) the marginal (prior) probability
  # of hypothesis being TRUE. But, in this case of almost 1500 schools, where "H"
  # is school si, P(E|not(H)) is approximately P(E) = sum(P(E|H)*P(H);over H). 
  # So, could skip this function and just use P(E).
  # 
  # ***IMPORTANT:*** I use log10 of the Bayes factors, which is a bit unusual,
  # but I know powers of 10 better than powers of 2 or *e*.
  
  cpNotH <- function(cpH,pH){
    pEH <- cpH * pH
    pE  <- sum(pEH)
    return((pE - pEH)/(1 - pH))
  }
  BFactor <- function(cpH,cpNotH,logFunc=log10){
    x <- cpH / cpNotH
    return(if(is.function(logFunc)) logFunc(x) else x)
  }
  makeCpNotH <- function(x) mapply(cpNotH,cpH=x,pH=ethnicity$probSchool)
  makeBF <- function(x) {x <- pmin(pmax(1.0E-9,x),1.0-1.0E-9);log10(x/sum(x*ethnicity$probSchool,na.rm=TRUE))} # Approximate
  

  ethnicBF <- ethnicity %>% 
    dplyr::select(contains("UGDS_")) %>% 
    mutate_each(funs(makeBF)) %>% 
    setNames(gsub("UGDS","BF",names(.))) %$%
    bind_cols(ethnicity[1:2],.) %>%
    mutate(
      BF_female_2005 = makeBF( student$female_2005 ),
      BF_male_2005   = makeBF( 1.0 - student$female_2005 ),
      BF_le24yrsold  = makeBF( 1.0 - student$UG25abv ),
      BF_gt24yrsold  = makeBF( student$UG25abv ),
      BF_veteran     = makeBF( student$veteran_2005 ),
      BF_notveteran  = makeBF( 1.0 - student$veteran_2005 ),
      BF_is1stgen    = makeBF( student$PAR_ED_PCT_1STGEN ),
      BF_not1stgen   = makeBF( 1.0 - student$PAR_ED_PCT_1STGEN )
    )
  #=====================================================
  
  ### Income: THESE VALUES ARE UNRELIABLE BECAUSE SO MANY MAJOR SCHOOLS HAVE MISSING OR ZERO VALUES.
  ###         USE DISTRIBUTIONS DERIVED FROM median_hh_inc_2005 INSTEAD.
  ###         ALSO ADD IN pct_born_us_2005 (for student origins) and loan_ever_2005 (for campus setting)
  
  income <- student %>% 
    dplyr::select(unitID,College,UGDS,DEP_STAT_PCT_IND,contains("INC_PCT")) %>%
    mutate(probSchool = UGDS/sum(UGDS),
           totprob      = rowSums(as.matrix(.[c("INC_PCT_LO","INC_PCT_M1","INC_PCT_M2","INC_PCT_H1",
                                                "INC_PCT_H2")])),
           totprobdep   = rowSums(as.matrix(.[c("DEP_INC_PCT_LO","DEP_INC_PCT_M1","DEP_INC_PCT_M2",
                                                "DEP_INC_PCT_H1","DEP_INC_PCT_H2")])),
           totprobind   = rowSums(as.matrix(.[c("IND_INC_PCT_LO","IND_INC_PCT_M1","IND_INC_PCT_M2",
                                                "IND_INC_PCT_H1","IND_INC_PCT_H2")])),
           totprob2dep  = rowSums(as.matrix(.[c("DEP_INC_PCT_LO","DEP_INC_PCT_H2")])),
           totprob2ind  = rowSums(as.matrix(.[c("IND_INC_PCT_LO","IND_INC_PCT_H2")])))
  #income %>% print(n=20)
  
  # To avoid dropping many schools due to missing values will hold 7 variables
  # for income: DEP_STAT_PCT_IND, DEP_INC_PCT_LO, DEP_INC_PCT_H2 &
  # DEP_INC_GTLO_LTH2, IND_INC_PCT_LO, IND_INC_PCT_H2 & IND_INC_GTLO_LTH2; where
  # the x_INC_GTLO_LTH2 variables are 1 - x_INC_PCT_H2 - x_INC_PCT_LO; and the
  # sum of the latter 6 columns = 1. So must mutate the current IND_... columns
  # by multiplying by DEP_STAT_PCT_IND. An "x" is appended to the end of the
  # newly computed variable names.
  
  income %<>% mutate(probSchool = UGDS/sum(UGDS),
                     DEP_STAT_PCT_INDx  = DEP_STAT_PCT_IND,
                     DEP_STAT_PCT_DEPx  = 1 - DEP_STAT_PCT_INDx,
                     DEP_INC_PCT_LOx    = DEP_INC_PCT_LO*DEP_STAT_PCT_DEPx,
                     DEP_INC_PCT_HI2x   = DEP_INC_PCT_H2*DEP_STAT_PCT_DEPx,
                     DEP_INC_GTLO_LTH2x = DEP_STAT_PCT_DEPx - DEP_INC_PCT_LOx - DEP_INC_PCT_HI2x,
                     IND_INC_PCT_LOx    = IND_INC_PCT_LO*DEP_STAT_PCT_INDx,
                     IND_INC_PCT_HI2x   = IND_INC_PCT_H2*DEP_STAT_PCT_INDx,
                     IND_INC_GTLO_LTH2x = DEP_STAT_PCT_IND - IND_INC_PCT_LOx - IND_INC_PCT_HI2x,
                     totprobx = DEP_INC_PCT_LOx + DEP_INC_PCT_HI2x + DEP_INC_GTLO_LTH2x + IND_INC_PCT_LOx
                     + IND_INC_PCT_HI2x + IND_INC_GTLO_LTH2x) %>%
    dplyr::select(unitID,College,UGDS,probSchool,matches("x$"))
  #income %>% print(n=20)
  #income[income$College=='California Institute of Technology',
  #        c('IND_INC_PCT_LOx','IND_INC_PCT_HI2x','IND_INC_GTLO_LTH2x','totprobx')] <- c(0,0,0,1)
  
  # Fit a lognormal to quantiles
  # Clumsily fit log-normal distributions to the upper & lower income quintiles for each school.
  fr <- function(x,INC_50,INC_low,INC_high,probs=c(0.20,0.80)) {
    sdl <- x[1]
    #q <- qlnorm(p=probs,meanlog=log(INC_MEAN) - sdl^2/2,sdlog=sdl)
    q <- qlnorm(p=probs,meanlog=log(INC_50),sdlog=sdl)
    log(INC_low/q[1])^2 + log(INC_high/q[2])^2
  }
  getMuSd <- function(INC_50,INC_low,INC_high,probs=c(0.20,0.80)) {
    
    INC_500 <- INC_50
    
    soln <- optim(c(1), fr,lower=1.0E-3,upper=2,method='L-BFGS-B',
                  INC_50=INC_50,INC_low=INC_low,INC_high=INC_high,probs=probs)
    sdl  <- soln$par
    # meanlog  <- log(INC_MEAN) - sdl^2/2
    meanlog  <- log(INC_50)
    # pINC_MEAN <- exp(meanlog+0.5*sdl^2)
    pINC_50 <- exp(meanlog)
    q   <- qlnorm(p=probs,meanlog=meanlog,sdlog=sdl)
    
    return(list(meanlog=meanlog,sdlog=sdl,pINC_low=q[1],pINC_50=pINC_50,pINC_high=q[2],convergence=soln$convergence,message=soln$message))
  }      
  
  a <- student  %>%
    dplyr::select(unitID,College,contains('DEP_INC_'),md_faminc_2005,median_hh_inc_2005) %>% 
    filter(!is.na(DEP_INC_PCT_LO) & !is.na(DEP_INC_PCT_H2)) %>% 
    mutate_at(funs(ifelse(.==0,1.0E-3,.)),.vars=vars(DEP_INC_PCT_LO,DEP_INC_PCT_H2)) %>% 
    rowwise() %>% 
    do(  
      { 
        tmp <- .
        with(tmp,{getMuSd(md_faminc_2005,30000,110000,c(DEP_INC_PCT_LO,1.0-DEP_INC_PCT_H2))}) %>% 
          as_data_frame() %>% 
          transmute(unitID=tmp$unitID, md_faminc_2005 = tmp$md_faminc_2005, sdlog = sdlog )
      } 
    ) %>%
    ungroup() %T>% 
    print(width=Inf)
  
  sdlogEst   <- a %>% right_join(student %>% dplyr::select(unitID,College) ,by='unitID') 
   
  
  sdlogLoess <- loess(sdlog ~ log(md_faminc_2005),data=a)
  sdlogPred  <- predict(sdlogLoess,newdata=sdlogEst)
  sdlogPred  <- data_frame(sdlogp = as.numeric(ifelse(is.na(sdlogEst$sdlog),sdlogPred,sdlogEst$sdlog)))
  sdlogEst %<>% bind_cols(sdlogPred)
  sdlogEst %<>%
    mutate(
      p_le30K        = plnorm(  30000,meanlog = log(md_faminc_2005),sdlog = sdlogp,lower.tail = TRUE ),
      p_gt30Kle48K   = plnorm(  48000,meanlog = log(md_faminc_2005),sdlog = sdlogp,lower.tail = TRUE ) -   p_le30K,
      p_gt48Kle75K   = plnorm(  75000,meanlog = log(md_faminc_2005),sdlog = sdlogp,lower.tail = TRUE ) - ( p_le30K + p_gt30Kle48K ),
      p_gt75Kle110K  = plnorm( 110000,meanlog = log(md_faminc_2005),sdlog = sdlogp,lower.tail = TRUE ) - ( p_le30K + p_gt30Kle48K + p_gt48Kle75K ),
      p_gt110K       = 1 - ( p_le30K + p_gt30Kle48K + p_gt48Kle75K + p_gt75Kle110K)
    )
  sdlogEst %<>% 
    bind_cols( income %>%  dplyr::select(probSchool) ) %>% 
    dplyr::select( unitID , College , probSchool , sdlogp , p_le30K , p_gt30Kle48K , p_gt48Kle75K , p_gt75Kle110K , p_gt110K ) 
  
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*sdlogEst$probSchool,na.rm=TRUE))} # Approximate
  bf     <- sdlogEst %>% mutate_each(funs(makeBF),starts_with('p_')) %>% setNames(sub('^p_','BF_',names(.)))
  
  # Check-in on some select schools:
  selectSchools <- paste(c('Massachusetts Institute of Technology', 'California Institute of Technology',
                           'Princeton', 'Yale', 'Harvard', 'Stanford', 'Duke', 'Vanderbilt', 'Berkeley',
                           'Northwestern U', 'Princeton', 'Cornell U', 'Ohio State U','University of Dayton'),collapse="|")
  sdlogEst %>% filter( grepl( selectSchools , College ) )
  
  
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*income$probSchool,na.rm=TRUE))} # Approximate
  incomeBF <- income %>% 
    dplyr::select( matches("x$"),-totprobx ) %>% 
    mutate_each( funs(makeBF) ) %>% 
    setNames( gsub("(.+)x$","BF_\\1x", names(.) ) ) %>%
    { bind_cols( income %>% select(1:4), . ) %>% inner_join( bf %>% select( unitID, starts_with('BF_') ), by='unitID' ) }
  
  #===================================================
  
  ### Student Aid:
  
  aid <- student %>% 
    dplyr::select(unitID,College,UGDS,matches("pell_ever|fsend")) %>%
    mutate(probSchool = UGDS/sum(UGDS),
           totprob    = rowSums(as.matrix(.[grepl('fsend',names(.))])))
  #aid %>% print(n=20)
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*aid$probSchool,na.rm=TRUE))} # Approximate
  aidBF <- aid %>% 
    dplyr::select(matches("pell_ever|fsend")) %>% 
    mutate_each(funs(makeBF)) %>% 
    setNames(paste0("BF_",names(.))) %$%
    bind_cols(aid[1:3],.)
  
  #       g <- aidBF %>% gather(key=Variable,value=BF,-(1:3)) %>%
  #         ggplot(aes(x=BF,fill=Variable))
  #       g + geom_density(alpha=0.3) + 
  #         facet_wrap(~Variable) + 
  #         scale_x_continuous(lim=c(-1,1)) + 
  #         theme(text=element_text(size=14,face = 'bold'))
  #===================================================
  
  ### Earnings after graduation (6 years on)
  
  earnRepay <- student %>% 
    dplyr::select(unitID,College,UGDS,matches("^(RPY|CDR|.+earn_wne_p6)")) %>%
    mutate(probSchool = UGDS/sum(UGDS),
           CDR3est       = (CDR3*UGDS + 10)/(UGDS + 10 + 140), # smooth with 150 hypothetical students having a nominal default rate of ~7%
           sdlog         = sqrt(log((sd_earn_wne_p6_2005/mn_earn_wne_p6_2005)^2+1)),
           meanlog       = log(mn_earn_wne_p6_2005^2/sqrt(mn_earn_wne_p6_2005^2+sd_earn_wne_p6_2005^2)),
           p_le30K       = plnorm(30.0E3,meanlog,sdlog),
           p_gt30Kle48K  = plnorm(48.0E3,meanlog,sdlog)  - p_le30K,
           p_gt48Kle75K  = plnorm(75.0E3,meanlog,sdlog)  - plnorm(48.0E3,meanlog,sdlog),
           p_gt75Kle110K = plnorm(110.0E3,meanlog,sdlog) - plnorm(75.0E3,meanlog,sdlog),
           p_gt110K      = plnorm(110.0E3,meanlog,sdlog,lower.tail=FALSE),
           totprob       = p_le30K + p_gt30Kle48K + p_gt48Kle75K + p_gt75Kle110K + p_gt110K) %>%
    dplyr::select(1:3,CDR3,CDR3est,5:7,probSchool,starts_with('p_'))
  
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*earnRepay$probSchool,na.rm=TRUE))} # Approximate
  earnRepayBF <- earnRepay %>% 
    dplyr::select(-c(1:3,9)) %>% 
    mutate_each(funs(makeBF)) %>% 
    setNames(paste0("BF_",names(.))) %$%
    bind_cols(earnRepay[c(1:3,9)],.) %>%
    filter(complete.cases(.))
  
  #       earnRepayBF %>% 
  #         dplyr::select(-3) %>% 
  #         inner_join(student %>% dplyr::select(unitID,ADM_RATE),by='unitID') %>% 
  #         filter(!is.na(ADM_RATE)) %>% 
  #         mutate(ADM_RATEd=discretize(.['ADM_RATE'],method='interval',breaks=6)[['ADM_RATE']]) %>% 
  #         arrange(desc(BF_p_gt110K)) %>% 
  #         filter(BF_p_le30K<0.5 & BF_p_gt110K>0.5) %$% 
  #         qplot(x=BF_p_le30K,y=BF_p_gt110K,color=ADM_RATEd) + geom_text(aes(label=College))
  #===================================================
  
  ### College Setting: locale & region.
  
  #       student %$% { locale %>% summary %>% print }
  #       student %$% { region %>% summary %>% print }
  
  localeNames <- sort(unique(as.character(student$locale)))
  localeAggregates <- setNames(c(gsub('([^ ]+) ([^ ]+).+','\\1\\2',localeNames[1:3]),
                                 rep('Rural',3),
                                 gsub('([^ ]+) ([^ ]+).+','\\1\\2',localeNames[7]),
                                 rep('Suburb:Small/Midsize & Town:Fringe',2),
                                 gsub('([^ ]+) ([^ ]+).+','\\1\\2',localeNames[10]),
                                 'Suburb:Small/Midsize & Town:Fringe',
                                 gsub('([^ ]+) ([^ ]+).+','\\1\\2',localeNames[12])),localeNames)
  student %<>% mutate(localeAgg = factor(localeAggregates[as.character(locale)],
                                         levels=c('Rural','Town:Remote','Town:Distant','Suburb:Small/Midsize & Town:Fringe',
                                                  'Suburb:Large','City:Small','City:Midsize','City:Large')))
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*settingBF$probSchool,na.rm=TRUE))} # Approximate
  settingBF <- student %>% 
    dplyr::select(unitID,College,UGDS,localeAgg,region) %>%
    mutate(probSchool = UGDS/sum(UGDS))
  a <- settingBF %$% model.matrix(~localeAgg - 1,data=.)
  localeColNames <- gsub('^([^:]+):*(.+)$','BF_\\1\\2',colnames(a))
  colnames(a) <- localeColNames
  locAbbr <- gsub('^.+Agg(.{4}).+','\\1',colnames(a))
  
  # Define similarity factors: This is a subjective probability (based on my opinion) determining how likely a student would attend college in
  # a locale (col) other than his/her most-preferred locale (row).  So each row sums to 1.
  fctr <- matrix(c(     1,  0.8, 0.01, 0.0001,
                      0.8,    1, 0.7 ,   0.01,
                     0.01,  0.7,    1,    0.8,
                   0.0001, 0.01,  0.8,      1)/2,nrow=4,ncol=4,byrow=TRUE, 
                 dimnames=list(key=c('Rura','Town','Subu','City'), other=c('Rura','Town','Subu','City')))
  # Expand factors to cover all locales.
  fctr <- fctr[locAbbr,locAbbr]
  fctr[row(fctr) == col(fctr)] <- 1 # put ones on the diagonal
  dimnames(fctr) <- list(localeColNames,localeColNames)
  
  # THIS NORMALIZATION WASN'T IN THE KAGGLE SCRIPTS:
  fctr %<>% sqrt # Here's a "flattening" transformation so that similarities are essentially increased (since all elements of fctr are <= 1.)
  fctr <- fctr / rowSums(fctr)
  
  # Reweight the local indicators.
  a <- a %*% fctr
  # Convert locale indicators intor psuedo-Bayes factors.
  a %<>% as.data.frame %>% tbl_df %>% mutate_each(funs(makeBF))
  
  
  settingBF %<>% dplyr::select(-localeAgg) %>% bind_cols(a)
  
  #regNames <- c('FarWest','GreatLakes','MidEast','NewEngland','Plains','RockyMountains','Southeast','Southwest')
  a <- settingBF %>%
    mutate(regionName = factor(gsub(' ','',as.character(student$region)))) %$% 
    model.matrix(~regionName - 1,data=.)
  # Define similarity factors
  fctrRegion <- matrix(c(1, 0.03, 0.0, 0.0, 0.1, 0.3, 0.0, 0.3,
                         0.03, 1, 0.3, 0.1, 0.3, 0.1, 0.03, 0.03,
                         0.0, 0.3, 1,  0.3, 0.01, 0.0, 0.1, 0.0,
                         0.0, 0.1, 0.3, 1, 0.0, 0.0, 0.0, 0.0,
                         0.1, 0.3, 0.01, 0.0, 1, 0.3, 0.1, 0.1,
                         0.3, 0.1, 0.0, 0.0, 0.3, 1, 0.0, 0.3,
                         0.0, 0.03, 0.1, 0.0, 0.1, 0.0, 1, 0.3,
                         0.3,0.03, 0.0, 0.0, 0.1, 0.3, 0.3, 1),nrow=8,ncol=8,byrow=TRUE,
                       dimnames=list(colnames(a),colnames(a)))
  a <- a %*% fctrRegion
  colnames(a) <- gsub('^regionName','BF_',colnames(a))
  a %<>% as.data.frame %>% tbl_df %>% mutate_each(funs(makeBF))
  settingBF %<>% dplyr::select(-region) %>% bind_cols(a)
  
  #===================================================
  ### Academics: Completion rates, admissions rates and SAT scores.
  # Took a look at scaled test scores, but ultimately settled upon the log-normal
  # fits to SATs below.
  # scaleSAT <- function(Score) (Score - 280)/(800 - 280)
  # scaleACT <- function(Score) (Score -   9)/( 36 -   9)
  # Impute missing values with the means
  #lmtest <- lm(ACTCMMID ~ SAT_AVG,data=student)
  
  # academicsBF <- student %>% 
  #   filter(!(isSpecialty=='TRUE' & is.na(ADM_RATE))) %>%
  #   dplyr::select(unitID,College,ADM_RATE,matches('(^C150)|(SAT|ACT)')) %>%
  #   filter(!is.na(C150_4_POOLED_SUPP)) %>%
  #   mutate(ADM_RATE = ifelse(is.na(ADM_RATE),mean(ADM_RATE,na.rm=TRUE),ADM_RATE)) %>%
  #   mutate(SAT_AVG_scaled  = scaleSAT(ifelse(is.na(SAT_AVG) , mean(SAT_AVG,na.rm=TRUE), SAT_AVG))) %>%
  #   mutate(ACTCMMID_scaled = scaleACT(ifelse(is.na(ACTCMMID),mean(ACTCMMID,na.rm=TRUE),ACTCMMID))) %>%
  #   dplyr::select(1:3,ncol(.) - (2:0))
  
  # a <- academicsBF %>% 
  #   mutate_each(funs(scaleSAT),starts_with('SAT'),-ends_with('scaled')) %>% 
  #   mutate_each(funs(scaleACT),starts_with('ACT'),-ends_with('scaled')) %>%
  #   gather(key=Test,value=scaledScore,-unitID,-College,-ADM_RATE)
  
  #***NOTE:*** This is a bit of a kludge, but it results in a distribution for SAT
  #Verbal+Math scores for each school, from which Bayes factors for 200-pt
  #intervals are computed. (Kludginess: Adding the quartiles of Verbal & Math
  #DOES NOT equal the quartiles of Verbal+Math.)
  
  student %>% 
    mutate(SAT_25=SATVR25+SATMT25,SAT_75=SATVR75+SATMT75) %>% 
    filter(!is.na(SAT_AVG) & !is.na(SAT_75)) %>% 
    ggplot(aes(x=SAT_AVG)) + geom_density(fill='orange',alpha=0.3) + 
    geom_density(aes(x=SAT_25),fill='red',alpha=0.1) + 
    geom_density(aes(x=SAT_75),fill='green',alpha=0.1)
  
  #*** CAPTURE WORCESTER POLYTECHNIC, WHICH IS "TEST-OPTIONAL" SO SAT AND ACT ARE BASED UPON SCORES OF ONLY THE
  #*** STUDENTS WHO SUBMITTED SCORES.  THEREFORE, EXPECT REPORTED SCORES ARE BIASED UPWARDS.
  # FROM THE WPI WEBSITE https://www.wpi.edu/admissions/undergraduate/what-we-look-for: 
  # "WPI is test-optional. Data only for students who submitted scores.
  #  SAT range (middle 50%): 1310–1480
  #  ACT range (middle 50%): 27–32"
  # so assume mid SAT is 1395 = avg. SAT
  # also assume math scores 110% of verbal scores, e.g. so verbal at 25%-tile = 1/2.1*1310 and math = 1310-verbal
  # finally, assume that total population would shift distn. down to about 90% of reported.
  math_verbal_ratio <- 1.1
  deflate <- 0.9
  
  wpi_sat_values <- (
    deflate*c( 
      1310/(1+math_verbal_ratio), 1480/(1+math_verbal_ratio), 
      1310*(1-1/(1+math_verbal_ratio)), 1480*(1-1/(1+math_verbal_ratio)),
      1310/(1+math_verbal_ratio), 1480/(1+math_verbal_ratio),
      (1310+1480)/2/(1+math_verbal_ratio), (1310+1480)/2*(1-1/(1+math_verbal_ratio)), (1310+1480)/2/(1+math_verbal_ratio), 
      (1310+1480)/2
    )
  ) %>% 
    setNames(c("SATVR25", "SATVR75", "SATMT25", "SATMT75", "SATWR25", "SATWR75", "SATVRMID", "SATMTMID", "SATWRMID", "SAT_AVG"))
  wpi_act_values <- (
    deflate*c( 
      27, 32, 
      27*2/(1+math_verbal_ratio), 32*2/(1+math_verbal_ratio),
      27*2*(1-1/(1+math_verbal_ratio)), 32*2*(1-1/(1+math_verbal_ratio)), 
      (27+32)/2,
      (27+32)/(1+math_verbal_ratio), (27+32)*(1-1/(1+math_verbal_ratio))
    )
  ) %>% 
    setNames(c("ACTCM25", "ACTCM75", "ACTEN25", "ACTEN75", "ACTMT25", "ACTMT75", "ACTCMMID", "ACTENMID", "ACTMTMID"))
  
  student[ grepl('Worcester Polytechnic Institute',student$College),names(wpi_sat_values) ] <- wpi_sat_values
  student[ grepl('Worcester Polytechnic Institute',student$College),names(wpi_act_values) ] <- wpi_act_values
  
  academics <- student %>% 
    mutate(SAT_25=SATVR25+SATMT25,SAT_75=SATVR75+SATMT75) %>% 
    filter(!is.na(SAT_AVG) & !is.na(SAT_75)) %>% 
    dplyr::select(
      unitID,College,UGDS,ADM_RATE,C150_4_POOLED_SUPP,SAT_25,SAT_AVG,SAT_75,matches("^C150_4_[^P]")
    ) %>%
    mutate( C150_4_OTHER = C150_4_POOLED_SUPP ) %>%
    mutate_at( funs(ifelse(is.na(.),C150_4_POOLED_SUPP,.)),.vars = vars(matches("^C150_4_[^P]")))
  
  
  # Clumsily fit log-normal distributions to the SAT score quartiles for each school.
  fr <- function(x,SAT_AVG,SAT_25,SAT_75,probs=c(0.25,0.75)) {
    sdl <- x[1]
    q <- qlnorm(p=probs,meanlog=log(SAT_AVG) - sdl^2/2,sdlog=sdl)
    log(SAT_25/q[1])^2 + log(SAT_75/q[2])^2
  }
  getMuSd <- function(SAT_AVG,SAT_25,SAT_75) {
    probs  <- c(0.25,0.75)
    SAT_AVG0 <- SAT_AVG
    if(SAT_75==1600) {
      probs   <- probs/(0.75+1.0E-5)
      SAT_AVG <- (SAT_AVG0 - 1600*(1-(0.75+1.0E-5)))/(0.75+1.0E-5)
    }
    soln <- optim(c(0.05), fr,lower=1.0E-3,upper=0.2,method='L-BFGS-B',
                  SAT_AVG=SAT_AVG,SAT_25=SAT_25,SAT_75=SAT_75,probs=probs)
    sdl  <- soln$par
    meanlog  <- log(SAT_AVG) - sdl^2/2
    pSAT_AVG <- exp(meanlog+0.5*sdl^2)
    q   <- qlnorm(p=probs,meanlog=meanlog,sdlog=sdl)
    if(SAT_75==1600){
      pSAT_AVG <- pSAT_AVG*(0.75+1.0E-5) + 1600*(1-(0.75+1.0E-5))
    }
    return(c(meanlog=meanlog,sdlog=sdl,pSAT_25=q[1],pSAT_AVG=pSAT_AVG,pSAT_75=q[2]))
  }
  
  # Get the log-normal parameters of the SAT distribution for each school
  academics <- academics %$% {bind_cols(.,as.data.frame(t(mapply(getMuSd,SAT_AVG,SAT_25,SAT_75))))}
  academics %>% 
    filter(grepl('Harvard|Cal.+Inst.+Tech|Mass.+Inst.+Tech|Princeton U|Northwestern U|Cornell U',College)) %>% 
    arrange(desc(SAT_AVG))
  
  # Discretize the SAT distributions
  academics %<>%
    filter(!is.na(C150_4_POOLED_SUPP)) %>%
    mutate(probSchool = UGDS/sum(UGDS),
           # 20160315: MLT #
           logPadmit_le800        = plnorm( 800, meanlog, sdlog, log.p = TRUE),
           logPadmit_gt800le1000  = plnorm(1000, meanlog, sdlog, log.p = TRUE),
           logPadmit_gt1000le1200 = plnorm(1200, meanlog, sdlog, log.p = TRUE),
           logPadmit_gt1200le1400 = plnorm(1400, meanlog, sdlog, log.p = TRUE),
           logPadmit_gt1400       = 0,
           
           p_le800        = plnorm( 800,meanlog,sdlog),
           p_gt800le1000  = plnorm(1000,meanlog,sdlog) - p_le800,
           p_gt1000le1200 = plnorm(1200,meanlog,sdlog) - plnorm(1000,meanlog,sdlog),
           p_gt1200le1400 = plnorm(1400,meanlog,sdlog) - plnorm(1200,meanlog,sdlog),
           p_gt1400       = plnorm(1400,meanlog,sdlog,lower.tail=FALSE),
           totprob        = p_le800 + p_gt800le1000 + p_gt1000le1200 + p_gt1200le1400 + p_gt1400)
  
  # NEW, 20160315: Add raw academics SAT log-cumulative-probabilities to the student data.frame.
  student %<>% inner_join(academics %>% dplyr::select(unitID,starts_with('logPadmit_')), by = 'unitID' )

  abase <- student %>% 
    dplyr::select(UGDS,contains('admit')) %>% 
    { 
      lapply(
        (.)[-1],
        function(x,tot) sum((10^x*(.)$UGDS)/tot) %>% log10, 
        tot=sum((.)$UGDS)
      ) 
    }
  student %<>% 
    mutate(
      logPadmit_le800        = logPadmit_le800 - abase$logPadmit_le800,
      logPadmit_gt800le1000  = logPadmit_gt800le1000 - abase$logPadmit_gt800le1000,
      logPadmit_gt1000le1200 = logPadmit_gt1000le1200 - abase$logPadmit_gt1000le1200,
      logPadmit_gt1200le1400 = logPadmit_gt1200le1400 - abase$logPadmit_gt1200le1400
    )
    
  # Compute Bayes Factors for the discretized SAT score distributions of each school
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*academics$probSchool,na.rm=TRUE))} # Approximate
  academicsBF <- academics %>% 
    dplyr::select(-c(1:3,matches('SAT|log|prob'))) %>% 
    mutate_each(funs(makeBF)) %>% 
    setNames(gsub("^p","BF_SAT",names(.))) %>%
    setNames(gsub("^([^B])","BF_\\1",names(.))) %$%
    bind_cols(academics[1:3],.)
  
  # Now add in the disciplines:
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*academics$probSchool,na.rm=TRUE))} # Approximate
  discplnsBF <-  academics %>% 
    dplyr::select(unitID,probSchool) %>% 
    inner_join(disciplines2013,by='unitID') %>%
    mutate_each(funs(makeBF),-seq_len(which(names(.) == discNames[1])-1)) %>% 
    setNames(c(setdiff(names(.),discNames),paste0('BF_',discNames)))

  # NEW, 20160313: Add raw discipline percentages to the student data.frame.
  student %<>% inner_join(disciplines2013 %>% dplyr::select(unitID,one_of(discNames)),by='unitID')
  
  academicsBF %<>% inner_join(discplnsBF %>% dplyr::select(unitID,starts_with('BF_')),by='unitID')
  b <- disciplines2013 %>% filter(sapply(unitID,function(ui) ui %in% academicsBF$unitID)) %>% dplyr::select(AgricultureAgriculture:length(.)) %>% tbl_df
  discEntropy <- data_frame(discBreadth = rowSums(-(b * log(b)),na.rm = TRUE), unitID = academicsBF$unitID) %>% mutate(BF_discBreadth = discBreadth - median(discBreadth))
  academicsBF %<>% inner_join(discEntropy,by='unitID')
  
  # Now add SAT BF columns that are adjusted for Challenge=Low and Challenge=High.
  # For Challenge=Low, just average SAT BF column at each SAT level with the level beneath it.
  # For Challenge=High, just average SAT BF column at each SAT level with the level above it.
  academicsBF %<>% 
    mutate(
      BF_SAT_800_HiChallenge       =  (BF_SAT_le800        + BF_SAT_gt800le1000 )/2,
      BF_SAT_800_1000_HiChallenge  =  (BF_SAT_gt800le1000  + BF_SAT_gt1000le1200)/2,
      BF_SAT_1000_1200_HiChallenge =  (BF_SAT_gt1000le1200 + BF_SAT_gt1200le1400)/2,
      BF_SAT_1200_1400_HiChallenge =  (BF_SAT_gt1200le1400 + BF_SAT_gt1400      )/2
    )
  
  # Adjust the Bayes Factor for the credit default rate with the completion rates by ethnicity because
  # (10 those default rates only hold if the student graduates and (2) completion rates vary significantly
  # by ethnicity even within the same college. (The more negative BF_CDR3est_xxx, the better.)
  earnRepayBF %<>%
    left_join( academics %>% dplyr::select( unitID, starts_with('C150_4_') ), by = 'unitID' ) %>%
    mutate_at( funs( pmin(1-1e-5,pmax(1e-5,.)) ), .vars = vars(starts_with('C150_4_') )) %>%
    # mutate_at( funs( ./(1-.) ), .vars = vars(starts_with('C150_4_') ) ) %>% # convert to odds             
    # mutate(
    #   BF_CDR3est_WHITE = BF_CDR3est + log10( C150_4_POOLED_SUPP/C150_4_WHITE ),
    #   BF_CDR3est_BLACK = BF_CDR3est + log10( C150_4_POOLED_SUPP/C150_4_BLACK ),
    #   BF_CDR3est_ASIAN = BF_CDR3est + log10( C150_4_POOLED_SUPP/C150_4_ASIAN ),
    #   BF_CDR3est_HISP  = BF_CDR3est + log10( C150_4_POOLED_SUPP/C150_4_HISP  ),
    #   BF_CDR3est_OTHER = BF_CDR3est + log10( C150_4_POOLED_SUPP/C150_4_OTHER ),
    #   BF_CDR3est_NRA   = BF_CDR3est + log10( C150_4_POOLED_SUPP/C150_4_NRA   )
    # ) %>%
    mutate(
      BF_CDR3est_WHITE = BF_CDR3est - log10( C150_4_WHITE ),
      BF_CDR3est_BLACK = BF_CDR3est - log10( C150_4_BLACK ),
      BF_CDR3est_ASIAN = BF_CDR3est - log10( C150_4_ASIAN ),
      BF_CDR3est_HISP  = BF_CDR3est - log10( C150_4_HISP  ),
      BF_CDR3est_OTHER = BF_CDR3est - log10( C150_4_OTHER ),
      BF_CDR3est_NRA   = BF_CDR3est - log10( C150_4_NRA   )
    ) %>%
    dplyr::select( - starts_with('C150_4_') )
 
  #===============================================================================
  # *** USE NET PRICE DISTRIBUTIONS TO ACCOUNT FOR COST OF EDUCATION/AFFORDABILITY
  #===============================================================================
  #netpriceBF
  netprice <- student %>% 
    select(
      unitID, 
      College, 
      CollegeType, 
      one_of( grep('NPT',names( student ),value = TRUE) ) 
    )
  
  # Public schools
  netpricePub <- netprice %>% 
    filter( CollegeType == 'Public') %>% 
    select( - ( grep('_PRIV',names(.) ) ) ) %>% 
    setNames( sub('_PUB','',names(.) ) ) %>%
    mutate( 
      NPTbase = NPT41,
      NPT45   = NA
    )
  # Private Schools
  netpricePrv <- netprice %>% 
    filter( CollegeType != 'Public') %>% 
    select( - ( grep('_PUB',names(.) ) ) ) %>% 
    setNames( sub('_PRIV','',names(.) ) ) %>%
    mutate( NPTbase = NPT41 )
  
  inData <- netpricePrv %>% bind_rows(netpricePub) %>%
    rename( outcome = NPT4 ) %>% 
    mutate( CollegeType = ifelse( CollegeType == 'Public' , 0.0, 1.0 ) ) %>%
    select( outcome, CollegeType, contains("NPT4")) %T>% print
  #'
  #' ## Profile Regression
  #' 
  #' Perform the profile regression, followed by determination of an optimal number of clusters.
  #+ profreg, cache = TRUE, autodep = TRUE
  randomSeed <- 131
  runInfoObj <- profRegr(
    seed     = randomSeed,
    yModel   = "Normal", 
    xModel   = "Normal", 
    nSweeps  = 10, 
    nClusInit= 20,
    nBurn    = 20, 
    data     = inData %>% as.data.frame , 
    output   = "output", 
    covNames = grep( 'NPT4.', names( inData ), value = TRUE ), 
    fixedEffectsNames = "CollegeType"
  )
  
  dissimObj <- calcDissimilarityMatrix( runInfoObj )
  clusObj   <- calcOptimalClustering( dissimObj )
  clusObj$nClusters
  clusObj$clusterSizes
  
  #'
  #' ## Post-Analysis Leveraging Cluster Labels for Imputation
  #' 
  #' Now append the cluster labels onto the netprice datasets.
  #' Plot the results to compare the profiles across clusters.
  #' 
  #+ clusterlabels, cache = TRUE, autodep = TRUE, warning = FALSE, message = FALSE
  netpricePrv %<>% 
    mutate( Cluster = factor(paste0('C',clusObj$clustering[1:n()])))
  netpricePub %<>% 
    mutate( Cluster = factor(paste0('C',clusObj$clustering[(nrow(netpricePrv)+1):nrow(inData)])))
  
  # Convert data.frame to long format.
  incbrkt <- c(
    NPT41 =     '0-30000',
    NPT42 = '30001-48000',
    NPT43 = '48001-75000',
    NPT44 = '75001-110000',
    NPT45 =       '110001+'
  )
  netpricePub2 <- netpricePub %>% 
    select(1:4, Cluster, NPTbase, everything() ) %>%
    gather(key=income,value=netprice,-(1:6)) %>% 
    mutate(income =  factor( incbrkt[ income ] , levels = unname( incbrkt ) ) )
  netpricePrv2 <- netpricePrv %>% 
    select(1:4, Cluster, NPTbase, everything() ) %>% 
    gather(key=income,value=netprice,-(1:6)) %>% 
    mutate(income =  factor( incbrkt[ income ] , levels = unname( incbrkt ) ) )
  
  # Given the clusters, we're going to impute missing net prices by using a cluster-averaged multiplier (fmult)
  # which captures the ratio of net price at one income level to that of the net price at the next lower income level.
  netprice2 <- netpricePrv2 %>% bind_rows(netpricePub2)
  
  multipliers <- netprice2 %>% mutate( income = as.numeric( income ) ) %>%
    group_by( Cluster ) %>% 
    do(
      mod  = loess( netprice ~ income, data = . , na.rm = TRUE )
    ) %>%
    mutate( 
      pred = list( predict(mod, data_frame( income = 1:5 ) )) )  %$%
      { mapply( 
        function( cl, pr ) 
          data_frame( 
            Cluster = cl, 
            income =   factor( incbrkt , levels = unname( incbrkt ) ), 
            netprice_pred = pr 
          ), 
        Cluster, 
        pred, 
        SIMPLIFY = FALSE 
      )
      } %>%
    lapply(
      function(x) {
        x %>% mutate( fmult = c(1,netprice_pred[-1]/netprice_pred[-n()]))
      }
    ) %>%
    { 
      do.call( bind_rows, . ) 
    }
  
  
  predtopprice <- netprice2 %>% left_join( multipliers , by = c('Cluster', 'income' ) ) %>%
    arrange( unitID, income ) %>% group_by( unitID ) %>%
    do( newprice = { aa<- .$netprice; while(any(is.na(aa))) { aa <- c(aa[1],aa[-length(aa)]) * .$fmult } ; aa } ) %>%
    ungroup %$% { unlist( newprice ) }
  
  netprice2 %<>%  
    arrange( unitID, income ) %>%
    mutate(
      netprice = ifelse(is.na(netprice),predtopprice,netprice),
      netprice = ifelse(netprice<1,1,netprice),
      pshft    = netprice-NPTbase
    )
  
  ntpbrkt <- c( "p_le10K", "p_le15K", "p_le20K", "p_le30K", "p_ignoreNetprice" ) %>% c(paste(.,"OUTOFSTATE",sep="_"))
  sdlogp <- c(0.2,0.2,0.25,0.27,0.3) %>% setNames( incbrkt )
  
  ntpsdlogEst <- netprice2 %>% left_join(student %>% select(unitID,starts_with("TUITIONFEE")), by = "unitID") %>%
    mutate(
      logOUTOFSTATE = log(TUITIONFEE_OUT/TUITIONFEE_IN),
      p_le10K  = plnorm(  10000, meanlog = log(netprice), sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ),
      p_le15K  = plnorm(  15000, meanlog = log(netprice), sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ),
      p_le20K  = plnorm(  20000, meanlog = log(netprice), sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ),
      p_le30K  = plnorm(  30000, meanlog = log(netprice), sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ),
      p_ignoreNetprice  = 1,
      p_le10K_OUTOFSTATE  = ifelse(CollegeType != "Public", p_le10K , plnorm(  10000, meanlog = log(netprice) + logOUTOFSTATE, sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ) ),
      p_le15K_OUTOFSTATE  = ifelse(CollegeType != "Public", p_le15K , plnorm(  15000, meanlog = log(netprice) + logOUTOFSTATE, sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ) ),
      p_le20K_OUTOFSTATE  = ifelse(CollegeType != "Public", p_le20K , plnorm(  20000, meanlog = log(netprice) + logOUTOFSTATE, sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ) ),
      p_le30K_OUTOFSTATE  = ifelse(CollegeType != "Public", p_le30K , plnorm(  30000, meanlog = log(netprice) + logOUTOFSTATE, sdlog = sdlogp[incbrkt[income]], lower.tail = TRUE ) ),
      p_ignoreNetprice_OUTOFSTATE  = 1
    )
  ntpsdlogEst %<>% 
    left_join( student %>%  select( unitID, UGDS ) , by = 'unitID' ) %>% 
    mutate( probSchool = UGDS/sum( UGDS, na.rm = TRUE ) ) %>%
    select( unitID , College, CollegeType , probSchool ,  income, p_le10K , p_le10K_OUTOFSTATE, p_le15K , p_le15K_OUTOFSTATE,
            p_le20K , p_le20K_OUTOFSTATE, p_le30K, p_le30K_OUTOFSTATE, p_ignoreNetprice, p_ignoreNetprice_OUTOFSTATE ) %>%
    mutate( 
      probSchool = ifelse(is.na(probSchool),mean(probSchool,na.rm=TRUE),probSchool),
      probSchool = probSchool/sum(probSchool)
    )
  
  ntpsdlogEst %>% group_by( income, CollegeType ) %>% summarize_each( funs(mean), starts_with("p_")) %>% arrange(CollegeType) %>% print
  
  incbrkt2 <- c(
    '0-30000'      =      'p_le30K',
    '30001-48000'  = 'p_gt30Kle48K',
    '48001-75000'  = 'p_gt48Kle75K',
    '75001-110000' = 'p_gt75Kle110K',
    '110001+'      = 'p_gt110K'
  )
  
  makeBF <- function(x) {x <- 1.0E-9 + x;log10(x/sum(x*ntpsdlogEst$probSchool,na.rm=TRUE))} # Approximate
  
  netpriceBF <- ntpsdlogEst %>% 
    gather(key=netprice,value=prop,starts_with("p_")) %>% 
    mutate(netprice = factor(netprice, levels = ntpbrkt) ) %>%
    arrange(unitID, income, netprice ) %>%
    mutate(
      income = factor( incbrkt2[ income ], levels = incbrkt2 ),
      keycol = factor( paste(income,netprice,sep="_"), levels = paste(income[1:50],netprice[1:50],sep="_") )
      # keycol = paste(income,netprice,sep="#")
    ) %>% 
    # arrange(unitID,income) %>% 
    select(-income,-netprice) %>% 
    spread(key=keycol,value=prop) %>%
    mutate_each(funs(makeBF),starts_with('p_')) %>% 
    setNames( sub( "p_","", sub( '^p_', 'BF_', names(.) ) ) )
  
  # # Check-in on some select schools:
  # selectSchools <- paste(c('Massachusetts Institute of Technology', 'California Institute of Technology',
  #                          'Princeton', 'Yale', 'Harvard', 'Stanford', 'Duke', 'Vanderbilt', 'Berkeley',
  #                          'Northwestern U', 'Princeton', 'Cornell U', 'Ohio State U','University of Dayton'),collapse="|")
  # for( inc in names( incbrkt2 ) ){
  #   ntpsdlogEst %>% filter( grepl( selectSchools , College ) , income == inc ) %>% print
  # }
  # 
  # for( inc in sub("p_","_",incbrkt2) ){
  #   netpriceBF %>% filter( grepl( selectSchools , College ) ) %>% select(College, matches(inc) ) %>% print
  # }  

  #================================================================================  
  
  # NOW: Build the model covariate data frame with all possible covariate
  # candidates. Each student's model only uses a subset of these columns.  See
  # function 'utility'....
  studentBF <- student     %>% dplyr::select( unitID , College , state , UGDS , ADM_RATE , TUITIONFEE_IN , TUITIONFEE_OUT ) %>%
    inner_join(incomeBF    %>% dplyr::select(-College,-UGDS), by='unitID') %>%
    inner_join(ethnicBF    %>% dplyr::select(-College),       by='unitID') %>%
    inner_join(aidBF       %>% dplyr::select(-College,-UGDS), by='unitID') %>%
    inner_join(settingBF   %>% dplyr::select(-College,-UGDS,-probSchool), by='unitID') %>%
    inner_join(earnRepayBF %>% dplyr::select(-College,-UGDS,-probSchool), by='unitID') %>%
    inner_join(academicsBF %>% dplyr::select(-College,-UGDS), by='unitID') %>%
    inner_join(netpriceBF  %>% dplyr::select( unitID, starts_with('BF' ) ), by = 'unitID' ) %>%
    mutate(BF_prior = log10(nrow(.)*probSchool)) 
  
  #     }
  #   }
  cat("Done.\n")
  
  # This maps the student profile property names to the college Bayes factor
  # data table ('studentBF') column names.
  propertyMap <- list(
    ethnicity  = c(
      white    = 'BF_WHITE',
      black    = 'BF_BLACK',
      hispanic = 'BF_HISP',
      asian    = 'BF_ASIAN',
      other    = 'BF_OTHER',
      foreign  = 'BF_NRA'
    ),
    income     = list(
      dependent   = c(
        le30K       = "BF_le30K",
        gt30Kle48K  = "BF_gt30Kle48K",
        gt48Kle75K  = "BF_gt48Kle75K",
        gt75Kle110K = "BF_gt75Kle110K",
        gt110K      = "BF_gt110K"
      ),
      independent = c(
        le30K       = "BF_le30K",
        gt30Kle48K  = "BF_gt30Kle48K",
        gt48Kle75K  = "BF_gt48Kle75K",
        gt75Kle110K = "BF_gt75Kle110K",
        gt110K      = "BF_gt110K"
      )
    ),
    sat        = list(
      satlowchallenge = c(
      le800        = 'BF_SAT_le800',  
        gt800le1000  = 'BF_SAT_800_HiChallenge', 
        gt1000le1200 = 'BF_SAT_800_1000_HiChallenge', 
        gt1200le1400 = 'BF_SAT_1000_1200_HiChallenge', 
        gt1400='BF_SAT_1200_1400_HiChallenge'
      ),
      satmedchallenge = c(
        le800        = 'BF_SAT_le800',  
      gt800le1000  = 'BF_SAT_gt800le1000', 
      gt1000le1200 = 'BF_SAT_gt1000le1200', 
      gt1200le1400 = 'BF_SAT_gt1200le1400', 
      gt1400='BF_SAT_gt1400'
    ),
      sathighchallenge = c(
        le800        = 'BF_SAT_800_HiChallenge',  
        gt800le1000  = 'BF_SAT_800_1000_HiChallenge', 
        gt1000le1200 = 'BF_SAT_1000_1200_HiChallenge', 
        gt1200le1400 = 'BF_SAT_1200_1400_HiChallenge', 
        gt1400='BF_SAT_gt1400'
      )
    ),
    discipline = setNames(paste0('BF_',discNames),discNames),
    discBreadth= c(
      entropy = 'BF_discBreadth'
    ),
    fasfa      = c(
      fsend_1 = 'BF_fsend_1_2005',
      fsend_2 = 'BF_fsend_2_2005',
      fsend_3 = 'BF_fsend_3_2005',
      fsend_4 = 'BF_fsend_4_2005',
      fsend_5 = 'BF_fsend_5_2005'
    ),
    region     = c(
      FarWest        = 'BF_FarWest(AK,CA,HI,NV,OR,WA)',
      RockyMountains = 'BF_RockyMountains(CO,ID,MT,UT,WY)',
      Southwest      = 'BF_Southwest(AZ,NM,OK,TX)',
      Plains         = 'BF_Plains(IA,KS,MN,MO,NE,ND,SD)',
      GreatLakes     = 'BF_GreatLakes(IL,IN,MI,OH,WI)',
      Southeast      = 'BF_Southeast(AL,AR,FL,GA,KY,LA,MS,NC,SC,TN,VA,WV)',
      MidEast        = 'BF_MidEast(DE,DC,MD,NJ,NY,PA)',
      NewEngland     = 'BF_NewEngland(CT,ME,MA,NH,RI,VT)' 
    ),
    locale     = c(
      Rural          = 'BF_localeAggRural',
      TownRemote     = 'BF_localeAggTownRemote',
      TownDistant    = 'BF_localeAggTownDistant',
      SuburbSmallMid = 'BF_localeAggSuburbSmall/Midsize & Town:Fringe',
      SuburbLarge    = 'BF_localeAggSuburbLarge',
      CitySmall      = 'BF_localeAggCitySmall',
      CityMidsize    = 'BF_localeAggCityMidsize',
      CityLarge      = 'BF_localeAggCityLarge'
    ),
    earnings   = c(
      le30K       = 'BF_p_le30K',  
      gt30Kle48K  = 'BF_p_gt30Kle48K', 
      gt48Kle75K  = 'BF_p_gt48Kle75K', 
      gt75Kle110K = 'BF_p_gt75Kle110K', 
      gt110K      = 'BF_p_gt110K' 
    ),
    dfltrate   = list(
        white    = 'BF_CDR3est_WHITE',
        black    = 'BF_CDR3est_BLACK',
        hispanic = 'BF_CDR3est_HISP',
        asian    = 'BF_CDR3est_ASIAN',
        other    = 'BF_CDR3est_OTHER',
        foreign  = 'BF_CDR3est_NRA'
    ),
    prior      = 'BF_prior',
    #completion = 'BF_C150_4_POOLED_SUPP',
    completion = list(
      white    = 'BF_C150_4_WHITE',
      black    = 'BF_C150_4_BLACK',
      hispanic = 'BF_C150_4_HISP',
      asian    = 'BF_C150_4_ASIAN',
      other    = 'BF_C150_4_OTHER',
      foreign  = 'BF_C150_4_NRA'
    ),
    admission  = 'BF_ADM_RATE',
    gender     = c(
      female = 'BF_female_2005',
      male   = 'BF_male_2005'
    ),
    firstgen = c(
      is1stgen  = 'BF_is1stgen',
      not1stgen = 'BF_not1stgen'
    ),
    militaryvet = c(
      veteran    = 'BF_veteran',
      notveteran = 'BF_notveteran'
    ),
    agerange    = c(
      le24yrsold = 'BF_le24yrsold',
      gt24yrsold = 'BF_gt24yrsold'
    ),
    netprice  = list(
      INSTATE = list(
        le30K       = c(      "BF_le30K_le10K",      "BF_le30K_le15K",      "BF_le30K_le20K",      "BF_le30K_le30K",      "BF_le30K_ignoreNetprice"),  
        gt30Kle48K  = c( "BF_gt30Kle48K_le10K", "BF_gt30Kle48K_le15K", "BF_gt30Kle48K_le20K", "BF_gt30Kle48K_le30K", "BF_gt30Kle48K_ignoreNetprice"), 
        gt48Kle75K  = c( "BF_gt48Kle75K_le10K", "BF_gt48Kle75K_le15K", "BF_gt48Kle75K_le20K", "BF_gt48Kle75K_le30K", "BF_gt48Kle75K_ignoreNetprice"), 
        gt75Kle110K = c("BF_gt75Kle110K_le10K","BF_gt75Kle110K_le15K","BF_gt75Kle110K_le20K","BF_gt75Kle110K_le30K","BF_gt75Kle110K_ignoreNetprice"), 
        gt110K      = c(     "BF_gt110K_le10K",     "BF_gt110K_le15K",     "BF_gt110K_le20K",     "BF_gt110K_le30K",     "BF_gt110K_ignoreNetprice") 
      ) %>% lapply( function(lst) {lst %>% setNames( sub('^p_','',ntpbrkt[1:(length(ntpbrkt)/2)]) )} ),
      OUTOFSTATE = list(
        le30K       = c(      "BF_le30K_le10K",      "BF_le30K_le15K",      "BF_le30K_le20K",      "BF_le30K_le30K",      "BF_le30K_ignoreNetprice"),  
        gt30Kle48K  = c( "BF_gt30Kle48K_le10K", "BF_gt30Kle48K_le15K", "BF_gt30Kle48K_le20K", "BF_gt30Kle48K_le30K", "BF_gt30Kle48K_ignoreNetprice"), 
        gt48Kle75K  = c( "BF_gt48Kle75K_le10K", "BF_gt48Kle75K_le15K", "BF_gt48Kle75K_le20K", "BF_gt48Kle75K_le30K", "BF_gt48Kle75K_ignoreNetprice"), 
        gt75Kle110K = c("BF_gt75Kle110K_le10K","BF_gt75Kle110K_le15K","BF_gt75Kle110K_le20K","BF_gt75Kle110K_le30K","BF_gt75Kle110K_ignoreNetprice"), 
        gt110K      = c(     "BF_gt110K_le10K",     "BF_gt110K_le15K",     "BF_gt110K_le20K",     "BF_gt110K_le30K",     "BF_gt110K_ignoreNetprice") 
      ) %>% lapply( function(lst) {lst %>% paste0("_OUTOFSTATE") %>% setNames( sub('^p_','',ntpbrkt[1:(length(ntpbrkt)/2)]) )} )
    )
  )
  
  makeTraitLabels <- function(trait,ntrait,levels){
    # Note that this must be called with positive integers for 'trait' &
    # 'ntrait' and ntrait <= length(levels) (with intention to use
    # ntrait==length(levels)) where 'levels' is a list of character vectors. 
    # Will return a character vector with length = prod(sapply(levels,length)).
    if(trait>=ntrait) return(levels[[trait]])
    return(c(sapply(levels[[trait]],paste0,makeTraitLabels(trait+1,ntrait,levels))))
  }
  traitLevels  <- list(
    Risk      = c('Lr','Mr','Hr'),
    Vision    = c('Lv','Mv','Hv'),
    Breadth   = c('Lb','Mb','Hb'),
    Challenge = c('Lc','Mc','Hc'))
  traitLabels  <- makeTraitLabels(trait=1,ntrait=length(traitLevels),levels=traitLevels)
  StudentTrait <- setNames(strsplit(traitLabels,'[a-z]'),traitLabels)
  
  #   show(studentBF %>% 
  #        filter(grepl('Harvard|Northwestern U|Mass.+Inst.+Tech.+|Cal.+Inst.+Tech.+',College)) %>% 
  #        dplyr::select(1:5,BF_Engineering,BF_localeAggCityLarge) %>% 
  #        as.data.frame)
  DataSpec <- list(studentBF   = studentBF,
                   student     = student,
                   propertyMap = propertyMap,
                   StudentTrait= StudentTrait,
                   discgrp     = discgrp,
                   sdlogEst    = sdlogEst
  )
  if( saveIt ) save(DataSpec, file = 'DataSpec.RData')
  return(DataSpec)
}
# ```
#' 
#===================================================================
# Function utility:
# Here's the Decision Analysis Model; Probabilistic, Utility-Based Discrete Choice Model
utility <- function(sBF,studentProfile,propertyMap,dump.covariates=FALSE){
  # studentBF must come in as a 1-row matrix!!!
  
  covarnames <- sapply(names(propertyMap),function(propnm){
    if(propnm == 'income'){
      return(propertyMap$income[[ifelse(studentProfile$dependent,
                                        'dependent',
                                        'independent')]][studentProfile[[propnm]]])
    }
    if(propnm == 'sat'){
      return( propertyMap$sat[[ which( c('L','M','H') == studentProfile$traits$Challenge ) ]] )
    }
    levelName <- studentProfile[[propnm]]
    if(is.null(levelName)) return(propertyMap[[propnm]]) 
    return(propertyMap[[propnm]][levelName])
  })
  if(is.null(dim(sBF))) {
    xBF <- matrix(sBF[covarnames],nrow=1,dimnames=list(NULL,covarnames))
  } else {
    xBF <- matrix(sBF[,covarnames],nrow=1,dimnames=list(NULL,covarnames))
  }
  
  # Assign useful column names...
  colnames(xBF) <- paste(colnames(xBF),names(covarnames),sep=':')
  
  #show(xBF)
  
  # Compute the utility the student derives from the school.
  #   show(studentProfile$beta)
  #   show(colnames(xBF))
  beta <- setNames(studentProfile$beta,colnames(xBF))
  #   beta <- c(ethnicity = 0.008976661, income = 0.008976661, sat = 0.403949731, discipline = 0.035008977, 
  #             fasfa = 0.026929982, region = 0.008976661, locale = 0.004488330, earnings = 0.044883303, 
  #             prior = 0.089766607, completion = 0.089766607, admission = -0.269299820, gender = -0.008976661)
  #show(beta)
  u    <- xBF %*% beta
  
  if (dump.covariates){
    wxBF <- xBF * matrix(beta,nrow=nrow(xBF),ncol=ncol(xBF),byrow = TRUE)
    colnames(wxBF) <- colnames(xBF)
    return(wxBF)
  } else {
    return(u[1])
  }
}

#================================================================

#' 
#' 
#+  setup, results='hide', echo=FALSE, message=FALSE, warning=FALSE,cache=TRUE,autodep=TRUE
#================================================================
# RUN THE SETUP FUNCTION:
DataSpec <- Setup( saveIt = TRUE , resetIt = TRUE)
DataSpec$utility <- utility

#================================================================

