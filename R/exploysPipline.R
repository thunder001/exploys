#sudo R CMD javareconf
options(java.parameters = "-Xmx3072m")

## -----------------------------------Functions---------------------------
#' This function combines two cohorts from Exploys
#' 
#' @param file1 A string of one downloaded cohort file path
#' @param file2 A string of another downloaded cohort file path
#' 
#' @return A named list and name is individual sheet name, such as "Diagnosis"
#' 
#' @export

filehandler <- function(file1, file2) {
  sheet_names <- readxl::excel_sheets(file1)
  sheet_num <- length(sheet_names)
  
  result_list <- vector("list", length=sheet_num)
  names(result_list) <- sheet_names[1:(sheet_num)]
  # handle first sheet
  f1 <- xlsx::read.xlsx2(file1, sheetIndex=1, startRow=2)
  f2 <- xlsx::read.xlsx2(file2, sheetIndex=1, startRow=2)
  result_list[[1]] <- merge(f1, f2, by="Organization", suffix=c("_group1", "_group2"), sort=FALSE)
  
  for (i in 2:(sheet_num)) {
    # handle multiple parts in one sheet, only keep "ever" term, exclude "last 3 years" and "last year"
    if (i > 9) {
      endrow = switch(i-9, 11183,2501, 41280, 1023, 2575, 9787, 13)
      f1 <- xlsx::read.xlsx2(file1, sheetIndex=i, startRow=2, endRow=endrow)
      f2 <- xlsx::read.xlsx2(file2, sheetIndex=i, startRow=2, endRow=endrow)
    }
    else {
      f1 <- xlsx::read.xlsx2(file1, sheetIndex=i, startRow=2)
      f2 <- xlsx::read.xlsx2(file2, sheetIndex=i, startRow=2)
    }
    # handle row number and row names difference
    result_list[[i]] <- merge(f1, f2, by="Description", suffix=c("_group1", "_group2"), sort=FALSE) 
    
  }
  return(result_list)
}


#' Perform Chi square goodness test to compute the significant catagory or sub-catagories 
#' with at least 100 observations 
#' 
#' @param catDF A dataframe of catagory from combined two cohorts
#' @param subcat A string set of sub-catagory, sucha as CVDs, default is null
#' @param cohortDF A dataframe containing cohort search terms
#' @param fdr multiple tests correcton threshold, default is 0.001
#' @param min_obs mininal number of observations required for comparson 
#' 
#' @return A dataframe with statitics test result
#' 
#' @export


getSigItems <- function(catDF,subcat=NULL, cohortDF, fdr=0.001, min_obs=10) {
  if (!is.null(subcat)) {
    catDF <- filter(catDF, Description %in% subcat)
  }
  catDF[, 2] <- as.numeric(as.character(catDF[, 2]))
  catDF[, 3] <- as.numeric(as.character(catDF[, 3]))
  catDF[, 4] <- as.numeric(as.character(catDF[, 4]))
  catDF[, 5] <- as.numeric(as.character(catDF[, 5]))
  cohort1_total <- as.numeric(as.character(cohortDF[2, 2]))
  #print(cohort1_total)
  cohort2_total <- as.numeric(as.character(cohortDF[2, 3]))
  #print(cohort2_total)
  total <- cohort1_total + cohort2_total
  null_prop <- c(cohort1_total/total, cohort2_total/total)
  #print(null_prop)
  
  
  chi_stat <- numeric(length=nrow(catDF))
  p_value <- numeric(length=nrow(catDF))
  phi_value <- numeric(length=nrow(catDF))
  for (i in 1:nrow(catDF)) {
    #cat(sprintf("%d of %d\n", i, nrow(catDF)))
    cat_first <- catDF[i, 2]
    cat_second <- catDF[i, 4]
    if (cat_first > min_obs & cat_second > min_obs) {
      chi <- chisq.test(c(cat_first, cat_second), p=null_prop)
      #print(chi)
      chi_stat[i] <- chi$statistic
      p_value[i] <- chi$p.value
      phi_value[i] <- sqrt(chi_stat[i] / (total))
    }
  }
  
  catDF$Chi_stat <- chi_stat
  catDF$p_value<- p_value
  catDF$adj_p_value <- p.adjust(catDF$p_value, method = "BH")
  catDF$phi <- phi_value
  
  # catDF_sig <- try(filter(catDF, adj_p_value < fdr & catDF[, 2] > min_obs & catDF[, 4] > min_obs) %>% 
  #                    arrange(adj_p_value))
  # if (is.null(catDF_sig)) {
  #   return(NULL)
  # }
  # return(catDF_sig)
  return(catDF)
}

#' This function perform contigency test for a catagory or subcatagory between two cohorts
#' 
#' @param catDF A dataframe of catagory from combined two cohorts
#' @subcat A string set of sub-catagory, sucha as CVDs, default is null
#' 
#' @return numeric value of p-value
#' 
#' @export

contigency_test <- function(catDF, subcat=NULL) {
  if (!is.null(subcat)) {
    catDF <- catDF[catDF$Description %in% subcat, ]
  }
  catDF[, 2] <- as.numeric(as.character(catDF[, 2]))
  catDF[, 3] <- as.numeric(as.character(catDF[, 3]))
  catDF[, 4] <- as.numeric(as.character(catDF[, 4]))
  catDF[, 5] <- as.numeric(as.character(catDF[, 5]))
  df <- catDF[, c(2,4)]
  p_value <- chisq.test(df)
  return(p_value)
}

#' This function computes odd ratio (OR) and confidence inteval (CI) using logistic regression
#' 
#' @param sigcatDF  A dataframe with statistics
#' @param subcat A string set of sub-catagory, sucha as CVDs, default is NA
#' @param cohortDF A dataframe containing cohort search terms
#' @param conf A beelean indication if confidence inteval will be computed, defalut is false due to 
#'     expensive computing.
#' 
#' @return A dataframe with odd ratio and confidence inteval
#' 
#' @export
#' 

or_conf <- function(sigcatDF, subcat=NULL, cohortDF, conf=FALSE) {
  if (!is.null(subcat)) {
    sigcatDF <- dplyr::filter(sigcatDF, Description %in% subcat)
  }
  OR <- numeric(nrow(sigcatDF))
  CONF_LOW <- numeric(nrow(sigcatDF))
  CONF_HIGH <- numeric(nrow(sigcatDF))
  
  cohort1_total <- as.numeric(as.character(cohortDF[2, 2]))
  cohort2_total <- as.numeric(as.character(cohortDF[2, 3]))
  
  for (i in 1:nrow(sigcatDF)) {
    cat(sprintf("%d of %d\n", i, nrow(sigcatDF)))
    cat_cohort1 <- c(rep(1,sigcatDF[i, 2]), rep(0, cohort1_total - sigcatDF[i, 2]))
    cat_cohort2 <- c(rep(1,sigcatDF[i, 4]), rep(0, cohort2_total - sigcatDF[i, 4]))
    logi_df <- data.frame(cat=c(cat_cohort1, cat_cohort2), cohorts=c(rep(0, cohort1_total), rep(1, cohort2_total)))
    glm_fit <- glm(cat~cohorts, family = "binomial", data=logi_df)
    OR[i] <- exp(summary(glm_fit)$coef[2,1])
    if (conf) {
      print("Computing confidence interval...")
      confi <- confint(glm_fit)
      CONF_LOW[i] <- exp(confi[2, 1])
      CONF_HIGH[i] <- exp(confi[2, 2])
    }
  }
  
  sigcatDF$OR <- OR
  if (conf) {
    sigcatDF$CONF_LOW <- CONF_LOW
    sigcatDF$CONF_HIGH <- CONF_HIGH
  }
  return(sigcatDF)
}

#' This is a wrapper function to streamline the analysis pipeline
#' 
#' @param group1 A string of excel filename
#' @param group2 A string of excel filename
#' @param category A string of comparison catelog, such as "Diagonosis"
#' @param subcat A string of comparison sub-catelog, such as "Heart failure"
#' @param or A boolean indicating if odd ratio will be computed, default is false
#' @param conf A boolean indicating if confidence inteval will be computed, defalut is false due to 
#'     expensive computing.
#' @param tests Type of statistic test, only "goodness" and "contigency" available
#' 
#' @return A dataframe of two group comparison results under "goodness" test. Otherwise a p-value will
#'   be given
#' 
#' @export
twoGroupComp <- function(group1, group2, category, subcat=NULL, or=TRUE, conf=FALSE, tests="goodness") {
  print("Combining two cohorts...")
  result <- filehandler(group1,group2)
  print("Combining done!")
  cat <- result[[category]]
  #print(head(cat))
  cohorts <- result[["Cohort Search terms"]]
  print("Computing statistics...")
  if (tests=="goodness") {
    diff_terms <- getSigItems(cat, subcat=subcat, cohorts)
    print("Computing statistics done!")
    if (or == FALSE) {
      return(diff_terms)
    }
    else {
      print("Computing odd ratio...")
      diff_terms_conf <- or_conf(diff_terms, subcat=subcat, cohorts, conf=conf)
      print("Done!")
      return(diff_terms_conf)
    }
  }
  if (tests=="contigency") {
    stat <- contigency_test(cat)
    return(stat)
  }
}



visualResult <- function(resultDF, resultFile=NULL) {
  if (!is.null(resultFile)) {
    resultDF <- read.csv(resultFile)
    resultDF <- resultDF[, -1]
  }
  
  pso_cvds <- arrange(resultDF, Description)
  pso_cvds <- pso_cvds[, c(3,5)]
  
  rownames(pso_cvds) <- c("AF","CAD","HF", "MI", "PVD")
  #names(pso_cvds) <- c("RDW_low/normal", "RDW_high")
  names(pso_cvds) <- c("Pso-", "Pso+")
  pso_cvds[] <- lapply(pso_cvds, as.character)
  pso_cvds[] <- lapply(pso_cvds, as.numeric)
  
  #main = "Adult without psoriasis\n-ex_biological-in_comobidities"
  #main = "Adult psoriasis patients\n-ex_biological-in_comobidities"
  #main = "CVDs in RDW_high\npso- vs pso+"
  #main = "CVDs in RDW_normal-low\npso- vs pso+"
  #main = "Prevelance of CVDs beteen RDW-norlow and RDW-high\n
   #in psoriasis treated with biologics"
  #main = "CVDs between pso- and pso_normal/low"
  #main = "CVDs between pso- and pso_high"
  #main = "CVDs between pso- and pso_bio_normal/low"
  #main = "CVDs between pso- and pso_bio_nor/low"
  main = "CVDs between pso- and pso_mtx_normal/low"
  #main = "CVDs between pso- and pso_mtx_high"
  
  barplot(t(as.matrix(pso_cvds)), main=main,
          beside = TRUE, col=c("darkblue", "red"),
          ylim=c(0, 15), ylab="Prevalence (%)", cex.lab = 1.5,
          legend = names(pso_cvds))
}

# plot_diagnosis <- function(diseases, cohort, pso_flag) {
#   
#   cohort_disease <- cohort[cohort$Description %in% diseases, ]
#   cohort_disease <- arrange(cohort_disease, Description)
#   rownames <- cohort_disease$Description
#   cohort_disease <- cohort_disease[, -1]
#   cohort_disease[] <- lapply(cohort_disease, function(x) as.numeric(as.character(x)))
#   cohort_disease <- cohort_disease[, c(2,4)]
#   rownames <- c("AF", "CAD", "HF", "MI", "PVD")
#   rownames(cohort_disease) <- rownames
#   names(cohort_disease) <- c("RDW_low/normal", "RDW_high")
#   if (pso_flag==1) {
#     main = "Adult psoriasis patients\n-ex_biological_comobidities"
#   }
#   else {
#     main = "Adult without psoriasis\n-ex_biological-in_comobidities "
#   }
#   barplot(t(as.matrix(cohort_disease)), main = main,
#           beside = TRUE, col=c("darkblue", "red"),
#           ylim=c(0, 30), ylab="Prevalence (%)", cex.lab = 1.5,
#           legend = names(cohort_disease))
# }

#' This is a wrapper function to streamline the pipeline
#' 
#' @param catDF A dataframe of catagory from combined two cohorts
#' @param subcat A string set of sub-catagory, sucha as CVDs, default is NULL
#' @tests A string of test type, only allows two values: "goodness" and "contigency"
#' 
#' @return A dataframe with full statistic results or a numeric p-value

# getStats <- function(catDF, subcat=NULL, cohortDF, conf=FALSE, tests="goodness") {
#   if (tests=="goodness") {
#     diff_terms <- getSigItems(catDF, subcat=subcat, cohortDF)
#     diff_terms_conf <- or_conf(diff_terms, subcat=subcat, conf=conf, cohortDF)
#     return(diff_terms_conf)
#   }
#   if (tests=="contigency") {
#     stat <- contigency_test(catDF)
#     return(stat)
#   }
# }

