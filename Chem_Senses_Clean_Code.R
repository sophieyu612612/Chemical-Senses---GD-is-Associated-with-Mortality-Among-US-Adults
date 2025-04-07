library(survival)
library(readr)
library(dplyr)
library(ggplot2)
library(psych) 
library(haven)
library(survey)
library(car)
library(tidyr)
library(dplyr)


################
#NHANES VERSION#
################

srvyin <- paste("NHANES_2013_2014_MORT_2019_PUBLIC.dat")   # full .DAT name here
srvyout <- "NHANES_2013_2014"

setwd("") #path for data file
CSQ <- read_xpt(file = "CSQ_H.XPT") #smell/taste questionnaire
CSX <- read_xpt(file = "CSX_H.XPT") #smell/taste test
DEMO <- read_xpt(file = "DEMO_H.XPT") #demographics
COT <- read_xpt(file = "COT_H.XPT") #cotinine 
CMD <- read_xpt(file = "MCQ_H.XPT") #comorbidities 
BPQ <- read_xpt(file = "BPQ_H.XPT") #cardiac risk factors
CFQ <- read_xpt(file = "CFQ_H.XPT") #cognitive function
DPQ <- read_xpt(file = "DPQ_H.XPT") #PHQ-9
DIQ <- read_xpt(file = "DIQ_H.XPT") #diabetes
BMI <- read_xpt(file = "BMX_H.XPT") #BMI


# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

str(dsn)

dsn <- mutate(dsn, SEQN = seqn)

#------ MERGING DATA -----------
test <- inner_join(DEMO, CSQ, by = "SEQN")%>%inner_join(CSX, by = "SEQN")%>%inner_join(COT, by = "SEQN")%>%inner_join(CMD, by = "SEQN")%>%inner_join(DIQ, by = "SEQN")%>%inner_join(BPQ, by = "SEQN")%>%inner_join(dsn, by = "SEQN")%>%inner_join(BMI, by = "SEQN")
test <- filter(test, CSQ080 != 7 & CSQ080 != 9) #excluded 2 patient with missing subjective taste
test <- filter(test, CSQ080 != 7 & CSQ080 != 9) #excluded 2 patient with missing subjective taste
test <- filter(test, BPQ020 != 7 & BPQ020 != 9 & BPQ080 != 7 & BPQ080 != 9) #excluded 33 patient with missing cardiac risk factors
test <- filter(test, MCQ160B != 9 & MCQ160C != 9 & MCQ160D != 9 & MCQ160E != 9 & MCQ160F != 9 & MCQ220 != 7 & MCQ220 != 9 & DIQ010 != 7 & DIQ010 != 9) #excluded 29 patient with missing cardiac and cancer history
test <- filter(test, DMDEDUC2 != 7 & DMDEDUC2 != 9) #removed 2 patients

test_2 <- select(test, c("SEQN", "CSQ010", "CSQ020", "CSQ040", "CSQ080", "CSQ100", "CSQ110", "CSXCHOOD", "CSXSBOD", "CSXSMKOD", "CSXLEAOD", "CSXSOAOD", "CSXSOAOD", "CSXGRAOD", "CSXONOD", "CSXNGSOD","CSXQUIPT", "CSXNAPT", "CSXQUIST", "CSXSLTST", "CSXSLTSG", "CSXNASG","CSXQUISG", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR",  "LBXCOT", "SDMVSTRA", "SDMVPSU", "WTMEC2YR", "BPQ020", "BPQ080", "CSQ240", "CSQ250", "MCQ160B", "MCQ160C","MCQ160D", "MCQ160E", "MCQ160F", "MCQ220", "DIQ010", "BMXBMI", "mortstat", "permth_int"))

test_3 <- na.omit(test_2) 

test_with_cog_dep <- inner_join(test, CFQ, by = "SEQN")%>%inner_join(DPQ, by = "SEQN")
test_with_cog_dep <- test_with_cog_dep %>% filter(!if_any(starts_with("DPQ"), ~ . %in% c(7, 9))) #excluded 17 patients who didn't know or refused phq-9 questions


test_4 <- select(test_with_cog_dep, c("SEQN", "CSQ010", "CSQ020", "CSQ040", "CSQ080", "CSQ240", "CSQ250", "CSQ100", "CSQ110", "CSXCHOOD", "CSXSBOD", "CSXSMKOD", "CSXLEAOD", "CSXSOAOD", "CSXSOAOD", "CSXGRAOD", "CSXONOD", "CSXNGSOD", "CSXQUIPT", "CSXNAPT", "CSXQUIST", "CSXSLTST", "CSXSLTSG", "CSXNASG","CSXQUISG", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR", 
                                      , "LBXCOT", "SDMVSTRA", "SDMVPSU", "WTMEC2YR", "BPQ020", "BPQ080", "MCQ160B", "MCQ160C","MCQ160D", "MCQ160E", "MCQ160F", "MCQ220", "DIQ010", "BMXBMI",  "mortstat", "permth_int", "CFDCST1", "CFDCST2", "CFDCST3", "CFDCSR", "CFDAST", "CFDDS",
                                      "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", "DPQ060", "DPQ070", "DPQ080", "DPQ090"))

test_5 <- na.omit(test_4)

fin_table <- mutate(test_5, RIAGENDR = ifelse(RIAGENDR == 1, "a. male", "b. female"))  %>%
  mutate(RIDRETH3 = ifelse(RIDRETH3 ==3, "a. Non-Hispanic White", "b. Other Race"))%>%
  mutate(INDFMPIR = ifelse(INDFMPIR < 2, "a. 0-1.99", ifelse(INDFMPIR < 4, "b. 2-3.99", ifelse(INDFMPIR<5, "e. 4-4.99", "f. >5"))))%>%
  mutate(BPQ020 = ifelse(BPQ020 == 1, "b. yes", "a. no"))  %>%
  mutate(BPQ080 = ifelse(BPQ080 == 1, "b. yes", "a. no"))  %>%
  mutate(MCQ160B = ifelse(MCQ160B == 1, "b. yes", "a. no"))  %>%
  mutate(MCQ160C = ifelse(MCQ160C == 1, "b. yes", "a. no"))  %>%
  mutate(MCQ160D = ifelse(MCQ160D == 1, "b. yes", "a. no"))  %>%
  mutate(MCQ160E = ifelse(MCQ160E == 1, "b. yes", "a. no"))  %>%
  mutate(MCQ160F = ifelse(MCQ160F == 1, "b. yes", "a. no"))  %>%
  mutate(MCQ220 = ifelse(MCQ220 == 1, "b. yes", "a. no"))  %>%
  mutate(DIQ010 = ifelse(DIQ010 == 1, "b. yes", "a. no"))  %>%
  mutate(Sub_Smell_Problem = ifelse(CSQ010 == 1 | CSQ040 == 1, 1, 0))  %>%
  mutate(Sub_Taste_Problem = ifelse(CSQ080 == 1 | CSQ110 == 1, 1, 0))%>%
  mutate(Whole_Taste_Problem = ifelse(CSXQUIST == 2 & CSXSLTST == 1, 0, 1))%>%
  mutate(Salt_Discrimination = ifelse (CSXSLTSG > CSXNASG, 1, 0)) %>%
  mutate(Quinine_Discrimination = ifelse (CSXSLTSG > CSXNASG, 1, 0)) %>%
  mutate(older_younger = ifelse(RIDAGEYR >= 65, "a. >= 65 years old", "b. 40 - 64 years old")) %>%
  mutate(CSXCHOOD = ifelse(CSXCHOOD == 2, 1, 0)) %>%
  mutate(CSXSBOD = ifelse(CSXSBOD == 1, 1, 0)) %>%
  mutate(CSXSMKOD = ifelse(CSXSMKOD == 3, 1, 0)) %>%
  mutate(CSXLEAOD = ifelse(CSXLEAOD == 3, 1, 0)) %>%
  mutate(CSXSOAOD = ifelse(CSXSOAOD == 1, 1, 0)) %>%
  mutate(CSXGRAOD = ifelse (CSXGRAOD == 2, 1, 0)) %>%
  mutate(CSXONOD = ifelse(CSXONOD == 3, 1, 0)) %>%
  mutate(CSXNGSOD = ifelse(CSXNGSOD == 4, 1, 0)) %>%
  mutate(UPSIT = CSXCHOOD + CSXSBOD + CSXSMKOD + CSXLEAOD + CSXSOAOD + CSXGRAOD + CSXONOD + CSXNGSOD) %>%
  mutate(Measured_Smell_Problem = ifelse(UPSIT >= 6, 0, 1)) %>%
  mutate(Isolated_Sub_Taste_Problem = ifelse(Sub_Taste_Problem == 1 & Sub_Smell_Problem == 0 & Measured_Smell_Problem == 0, "b. yes", "a. no")) %>%
  mutate(Isolated_Whole_Taste_Problem = ifelse(Whole_Taste_Problem == 1 & Sub_Smell_Problem == 0 & Measured_Smell_Problem == 0, "b. yes", "a. no"))%>%
  mutate(Sub_Taste_Smell_Problem = ifelse(Sub_Taste_Problem == 1 & (Sub_Smell_Problem == 1 | Measured_Smell_Problem == 1), "b. yes", "a. no")) %>%
  mutate(Whole_Taste_Smell_Problem = ifelse(Whole_Taste_Problem == 1 & (Sub_Smell_Problem == 1 | Measured_Smell_Problem == 1), "b. yes", "a. no"))%>%
  mutate(Isolated_Smell_Problem = ifelse(Whole_Taste_Problem == 0 & Sub_Taste_Problem == 0 & (Sub_Smell_Problem == 1 | Measured_Smell_Problem == 1), "b. yes", "a. no")) %>%
  mutate(Combined_Problem = ifelse((Whole_Taste_Problem == 1 | Sub_Taste_Problem == 1) & (Sub_Smell_Problem == 1 |  Measured_Smell_Problem == 1), "b. yes", "a. no"))%>%
  mutate(Combined_GD_Sub_Smell = ifelse((Whole_Taste_Problem == 1 | Sub_Taste_Problem == 1) & (Sub_Smell_Problem == 1), "b. yes", "a. no"))%>%
  mutate(Combined_GD_Obj_Smell = ifelse((Whole_Taste_Problem == 1 | Sub_Taste_Problem == 1) & (Measured_Smell_Problem == 1), "b. yes", "a. no"))%>%
  mutate(Isolated_Bitter_Problem = ifelse(Isolated_Whole_Taste_Problem == "b. yes" & CSXQUIST == 2, "b. yes", "a. no")) %>%
  mutate(Isolated_Salt_Problem = ifelse(Isolated_Whole_Taste_Problem == "b. yes" & CSXSLTST == 1, "b. yes", "a. no")) %>%
  mutate(PHQ = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090)%>%
  mutate(PHQ_cat = ifelse(PHQ <= 4, "a. none", ifelse(PHQ <=9, "b. mild", ifelse(PHQ <= 14, "c. moderate", ifelse(PHQ <= 19, "d. moderately severe", ifelse(PHQ <= 27, "e. severe", "f. other"))))))%>%
  mutate(CERAD_IR = (CFDCST1 + CFDCST2 + CFDCST3)/3)%>% #immediate recall
  mutate(CERAD_DR = CFDCSR)%>% #delayed recall
  mutate(digital_symbol_score = CFDDS)

fin_table$survival <- Surv(time = fin_table$permth_int, event = fin_table$mortstat)
finalized_table<- svydesign(ids = ~SDMVPSU, data = fin_table, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR)

fin_table_older <- filter(fin_table, RIDAGEYR >=65)
fin_table_older$survival <- Surv(time = fin_table_older$permth_int, event = fin_table_older$mortstat)
finalized_table_older<- svydesign(ids = ~SDMVPSU, data = fin_table_older, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR)

fin_table_young <- filter(fin_table, RIDAGEYR < 65)
fin_table_young$survival <- Surv(time = fin_table_young$permth_int, event = fin_table_young$mortstat)
finalized_table_young<- svydesign(ids = ~SDMVPSU, data = fin_table_young, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR)

fin_table_male <- filter(fin_table, RIAGENDR == "a. male")
fin_table_male$survival <- Surv(time = fin_table_male$permth_int, event = fin_table_male$mortstat)
finalized_table_male<- svydesign(ids = ~SDMVPSU, data = fin_table_male, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR)

fin_table_female <- filter(fin_table, RIAGENDR == "b. female")
fin_table_female$survival <- Surv(time = fin_table_female$permth_int, event = fin_table_female$mortstat)
finalized_table_female<- svydesign(ids = ~SDMVPSU, data = fin_table_female, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR)

#Table 1 - Demographics
#====================
table(fin_table$Isolated_Sub_Taste_Problem)
table(fin_table$Isolated_Whole_Taste_Problem)
table(fin_table$Isolated_Smell_Problem)
table(fin_table$Sub_Taste_Smell_Problem)
table(fin_table$Measured_Smell_Problem)
table(fin_table$Whole_Taste_Smell_Problem)
table(fin_table$Combined_Problem)

contingency_table <- table(fin_table$Isolated_Sub_Taste_Problem, fin_table$Isolated_Whole_Taste_Problem)
dimnames(contingency_table) <- list(
  "Isolated Sub Taste Problem" = c("a. no", "b. yes"),
  "Isolated Whole Taste Problem" = c("a. no", "b. yes")
)
print("Contingency Table:")
print(contingency_table)

# Step 2: Perform Chi-square test
chisq_result <- chisq.test(contingency_table)
print("Chi-square Test Result:")
print(chisq_result)

# Step 3: Calculate the Odds Ratio
# Extract the values from the contingency table
a <- contingency_table[1, 1]  # no/no
b <- contingency_table[1, 2]  # no/yes
c <- contingency_table[2, 1]  # yes/no
d <- contingency_table[2, 2]  # yes/yes

# Calculate the Odds Ratio
odds_ratio <- (a * d) / (b * c)
print(paste("Odds Ratio:", round(odds_ratio, 2)))

contingency_table <- table(fin_table$Combined_Problem, fin_table$Isolated_Whole_Taste_Problem)
dimnames(contingency_table) <- list(
  "Combined Taste Problem" = c("a. no", "b. yes"),
  "Isolated Whole Taste Problem" = c("a. no", "b. yes")
)
print("Contingency Table:")
print(contingency_table)

# Step 2: Perform Chi-square test
chisq_result <- chisq.test(contingency_table)
print("Chi-square Test Result:")
print(chisq_result)

# Step 3: Calculate the Odds Ratio
# Extract the values from the contingency table
a <- contingency_table[1, 1]  # no/no
b <- contingency_table[1, 2]  # no/yes
c <- contingency_table[2, 1]  # yes/no
d <- contingency_table[2, 2]  # yes/yes

# Calculate the Odds Ratio
odds_ratio <- (a * d) / (b * c)
print(paste("Odds Ratio:", round(odds_ratio, 2)))

# Initialize the total count
total_n <- 1136

# Create summary statistics for continuous variables
summary_table <- data.frame(
  Variable = c("RIDAGEYR (Age)", "LBXCOT (Cotinine)", "BMXBMI (BMI)"),
  Summary = c(
    paste("Median:", median(fin_table$RIDAGEYR, na.rm = TRUE), 
          "| IQR:", IQR(fin_table$RIDAGEYR, na.rm = TRUE)),
    paste("Median:", median(fin_table$LBXCOT, na.rm = TRUE), 
          "| IQR:", IQR(fin_table$LBXCOT, na.rm = TRUE)),
    paste("Median:", median(fin_table$BMXBMI, na.rm = TRUE), 
          "| IQR:", IQR(fin_table$BMXBMI, na.rm = TRUE))
  )
)

# Function to format categorical data with counts and percentages
get_cat_summary <- function(var_name, var_data) {
  tbl <- as.data.frame(table(var_data))
  tbl$Percent <- round((tbl$Freq / total_n) * 100, 2)
  tbl <- tbl %>%
    mutate(
      Variable = var_name,
      Summary = paste0(Freq, " (", Percent, "%)")
    ) %>%
    select(Variable, Summary)
  return(tbl)
}

# Append categorical variable summaries
summary_table <- rbind(
  summary_table,
  get_cat_summary("RIAGENDR (Gender)", fin_table$RIAGENDR),
  get_cat_summary("RIDRETH3 (Race/Ethnicity)", fin_table$RIDRETH3),
  get_cat_summary("INDFMPIR (Income-PIR)", fin_table$INDFMPIR),
  get_cat_summary("MCQ160C (Coronary Artery Disease)", fin_table$MCQ160C),
  get_cat_summary("MCQ160E (Heart Attack)", fin_table$MCQ160E),
  get_cat_summary("MCQ220 (Cancer)", fin_table$MCQ220),
  get_cat_summary("DIQ010 (Diabetes)", fin_table$DIQ010),
  get_cat_summary("Isolated Measured GD", fin_table$Isolated_Whole_Taste_Problem),
  get_cat_summary("mortstat (Mortality Status)", fin_table$mortstat)
)

# Display the summary table
summary_table

write.csv(summary_table, "demographics.csv", row.names = FALSE)

#-------------- Table 1: Demographics - Subgroups -----------------

demo_table_group_no <- fin_table[fin_table$Isolated_Salt_Problem == "a. no", ]
demo_table_group_yes <- fin_table[fin_table$Isolated_Salt_Problem == "b. yes", ]
table(fin_table$Isolated_Salt_Problem)

total_n <- 109 #demonimator, total number of participants in the below demo_table_group
table_demo <- demo_table_group_yes

# Create summary statistics for continuous variables
summary_table <- data.frame(
  Variable = c("RIDAGEYR (Age)", "LBXCOT (Cotinine)", "BMXBMI (BMI)"),
  Summary = c(
    paste("Median:", median(table_demo$RIDAGEYR, na.rm = TRUE), 
          "| IQR:", IQR(table_demo$RIDAGEYR, na.rm = TRUE)),
    paste("Median:", median(table_demo$LBXCOT, na.rm = TRUE), 
          "| IQR:", IQR(table_demo$LBXCOT, na.rm = TRUE)),
    paste("Median:", median(table_demo$BMXBMI, na.rm = TRUE), 
          "| IQR:", IQR(table_demo$BMXBMI, na.rm = TRUE))
  )
)

# Function to format categorical data with counts and percentages
get_cat_summary <- function(var_name, var_data) {
  tbl <- as.data.frame(table(var_data))
  tbl$Percent <- round((tbl$Freq / total_n) * 100, 2)
  tbl <- tbl %>%
    mutate(
      Variable = var_name,
      Summary = paste0(Freq, " (", Percent, "%)")
    ) %>%
    select(Variable, Summary)
  return(tbl)
}

# Append categorical variable summaries
summary_table <- rbind(
  summary_table,
  get_cat_summary("RIAGENDR (Gender)", table_demo$RIAGENDR),
  get_cat_summary("RIDRETH3 (Race/Ethnicity)", table_demo$RIDRETH3),
  get_cat_summary("INDFMPIR (Income-PIR)", table_demo$INDFMPIR),
  get_cat_summary("MCQ160C (Coronary Artery Disease)", table_demo$MCQ160C),
  get_cat_summary("MCQ160E (Heart Attack)", table_demo$MCQ160E),
  get_cat_summary("MCQ220 (Cancer)", table_demo$MCQ220),
  get_cat_summary("DIQ010 (Diabetes)", table_demo$DIQ010),
  get_cat_summary("Isolated Measured GD", table_demo$Isolated_Whole_Taste_Problem),
  get_cat_summary("mortstat (Mortality Status)", table_demo$mortstat)
)
write.csv(summary_table, "demographics.csv", row.names = FALSE)

#-------------- Table 1: Demographics - Significance -----------------
comparison_categories <- c("Isolated_Sub_Taste_Problem", "Isolated_Whole_Taste_Problem", "Combined_Problem")
continuous_vars <- c("RIDAGEYR", "LBXCOT", "BMXBMI")
categorical_vars <- c("RIAGENDR", "RIDRETH3", "INDFMPIR", "MCQ160C", "MCQ160E", "MCQ220", "DIQ010", "mortstat")

results_list <- list()

for (comparison in comparison_categories) {
  group_no <- fin_table[fin_table[[comparison]] == "a. no", ]
  group_yes <- fin_table[fin_table[[comparison]] == "b. yes", ]
  
  # Wilcoxon tests for continuous vars
  for (var in continuous_vars) {
    p <- wilcox.test(group_no[[var]], group_yes[[var]])$p.value
    formatted_p <- ifelse(p < 0.001, "p<0.001", round(p, 3))
    results_list[[length(results_list) + 1]] <- data.frame(
      Comparison = comparison,
      Variable = var,
      P_Value = formatted_p
    )
  }
  
  # Chi-square tests for categorical vars
  for (var in categorical_vars) {
    tbl <- table(fin_table[[comparison]], fin_table[[var]])
    p <- chisq.test(tbl)$p.value
    formatted_p <- ifelse(p < 0.001, "p<0.001", round(p, 3))
    results_list[[length(results_list) + 1]] <- data.frame(
      Comparison = comparison,
      Variable = var,
      P_Value = formatted_p
    )
  }
}
p_values_df <- do.call(rbind, results_list)
print(p_values_df)

write.csv(p_values_df, "p_values.csv", row.names = FALSE)


#------------- Identifying significant covariates Subjective and Objective Taste Issues ------------------

#Subjective Taste Issues:INDFMPIR + LBXCOT + CMD_score
#Objective Taste Issues: 
# RIAGENDR + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + DR1TKCAL  + CSQ240 + CSQ250 + CMD_score

model <- svyglm(Obj_Taste_Problem ~ CMD_score, design = finalized_table, family = gaussian) #binomial versus gaussian
summary(model)
conf_interval <- confint(model, level = 0.95)
odds_ratios <- exp(coef(model))
lower_ci <- exp(conf_interval[, "2.5 %"])
upper_ci <- exp(conf_interval[, "97.5 %"])
results <- data.frame(
  "Odds Ratio" = odds_ratios,
  "Lower CI" = lower_ci,
  "Upper CI" = upper_ci
)
print(results)


#Table 2, Supplemental Table 2, Supplemental Table 3 - Weighted Cox Proportional Hazards Regression
#=============================================================================
#Table 2
models <- list(
  "Isolated_Sub_Taste_Problem" = "survival ~ Isolated_Sub_Taste_Problem",
  "Isolated_Whole_Taste_Problem" = "survival ~ Isolated_Whole_Taste_Problem",
  "Combined_Problem" = "survival ~ Combined_Problem",
  "Isolated_Sub_Taste_Problem_1" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Isolated_Whole_Taste_Problem_1" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Combined_Problem_1" = "survival ~ Combined_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Isolated_Sub_Taste_Problem_2" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Isolated_Whole_Taste_Problem_2" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Combined_Problem_2" = "survival ~ Combined_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Isolated_Sub_Taste_Problem_full" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Isolated_Whole_Taste_Problem_full" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Combined_Problem_full" = "survival ~ Combined_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

#Supplemental Table 2
models <- list(
  "Sub_Smell_Problem" = "survival ~ Sub_Smell_Problem",
  "Measured_Smell_Problem" = "survival ~ Measured_Smell_Problem",
  "Sub_Smell_Problem_1" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Measured_Smell_Problem_1" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Sub_Smell_Problem_2" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Measured_Smell_Problem_2" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Sub_Smell_Problem_full" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Measured_Smell_Problem_full" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

#Supplemental Table 3
models <- list(
  "Sub_Taste_Problem" = "survival ~ Sub_Taste_Problem",
  "Whole_Taste_Problem" = "survival ~ Whole_Taste_Problem",
  "Sub_Taste_Problem_1" = "survival ~ Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Whole_Taste_Problem_1" = "survival ~ Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Sub_Taste_Problem_2" = "survival ~ Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Whole_Taste_Problem_2" = "survival ~ Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Sub_Taste_Problem_full" = "survival ~ Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Whole_Taste_Problem_full" = "survival ~ Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

# Initialize a table to store the results
results_table <- data.frame()

# Loop through each model and extract results
for (model_name in names(models)) {
  # Fit the model
  model <- svycoxph(as.formula(models[[model_name]]), design = finalized_table)
  
  # Tidy the model output
  tidy_model <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # Extract and format the required values
  tidy_model <- tidy_model %>%
    mutate(
      model = model_name,
      HR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), " - ", round(conf.high, 2), ")"),  # Combine HR and CI
      p_value = round(p.value, 3)
    ) %>%
    select(model, term, HR_CI, p_value)  # Select relevant columns
  
  # Append to the results table
  results_table <- bind_rows(results_table, tidy_model)
}

# View the results table
print(results_table)
write.csv(results_table, "cox_model_results.csv", row.names = FALSE)

#=============================================================================
#Table 2
models <- list(
  "Isolated_Sub_Taste_Problem" = "survival ~ Isolated_Sub_Taste_Problem",
  "Isolated_Whole_Taste_Problem" = "survival ~ Isolated_Whole_Taste_Problem",
  "Combined_Problem" = "survival ~ Combined_Problem",
  "Isolated_Sub_Taste_Problem_1" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Isolated_Whole_Taste_Problem_1" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Combined_Problem_1" = "survival ~ Combined_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Isolated_Sub_Taste_Problem_2" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Isolated_Whole_Taste_Problem_2" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Combined_Problem_2" = "survival ~ Combined_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Isolated_Sub_Taste_Problem_full" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Isolated_Whole_Taste_Problem_full" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Combined_Problem_full" = "survival ~ Combined_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

#Supplemental Table 2
models <- list(
  "Sub_Smell_Problem" = "survival ~ Sub_Smell_Problem",
  "Measured_Smell_Problem" = "survival ~ Measured_Smell_Problem",
  "Sub_Smell_Problem_1" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Measured_Smell_Problem_1" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Sub_Smell_Problem_2" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Measured_Smell_Problem_2" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Sub_Smell_Problem_full" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Measured_Smell_Problem_full" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

table(fin_table$Whole_Taste_Problem)

#Supplemental Table 3
models <- list(
  "Sub_Taste_Problem" = "survival ~ Sub_Taste_Problem",
  "Whole_Taste_Problem" = "survival ~ Whole_Taste_Problem",
  "Sub_Taste_Problem_1" = "survival ~ Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Whole_Taste_Problem_1" = "survival ~ Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Sub_Taste_Problem_2" = "survival ~ Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Whole_Taste_Problem_2" = "survival ~ Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Sub_Taste_Problem_full" = "survival ~ Sub_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Whole_Taste_Problem_full" = "survival ~ Whole_Taste_Problem + RIDAGEYR + RIAGENDR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)
# Initialize a table to store the results
results_table <- data.frame()

# Loop through each model and extract results
for (model_name in names(models)) {
  # Fit the model
  model <- svycoxph(as.formula(models[[model_name]]), design = finalized_table_young)
  
  # Tidy the model output
  tidy_model <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # Extract and format the required values
  tidy_model <- tidy_model %>%
    mutate(
      model = model_name,
      HR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), " - ", round(conf.high, 2), ")"),  # Combine HR and CI
      p_value = round(p.value, 3)
    ) %>%
    select(model, term, HR_CI, p_value)  # Select relevant columns
  
  # Append to the results table
  results_table <- bind_rows(results_table, tidy_model)
}

# View the results table
print(results_table)
write.csv(results_table, "cox_model_results.csv", row.names = FALSE)

#=================Subgroup Analysis - Age and Sex=================================
models <- list(
  "Isolated_Sub_Taste_Problem" = "survival ~ Isolated_Sub_Taste_Problem",
  "Isolated_Whole_Taste_Problem" = "survival ~ Isolated_Whole_Taste_Problem",
  "Combined_Problem" = "survival ~ Combined_Problem",
  "Isolated_Sub_Taste_Problem_1" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT",
  "Isolated_Whole_Taste_Problem_1" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT",
  "Combined_Problem_1" = "survival ~ Combined_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT",
  "Isolated_Sub_Taste_Problem_2" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT+ MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Isolated_Whole_Taste_Problem_2" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Combined_Problem_2" = "survival ~ Combined_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Isolated_Sub_Taste_Problem_full" = "survival ~ Isolated_Sub_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Isolated_Whole_Taste_Problem_full" = "survival ~ Isolated_Whole_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Combined_Problem_full" = "survival ~ Combined_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

#Supplemental Table 2
models  <- list(
  "Sub_Smell_Problem" = "survival ~ Sub_Smell_Problem",
  "Measured_Smell_Problem" = "survival ~ Measured_Smell_Problem",
  "Sub_Smell_Problem_1" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Measured_Smell_Problem_1" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Sub_Smell_Problem_2" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Measured_Smell_Problem_2" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Sub_Smell_Problem_full" = "survival ~ Sub_Smell_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Measured_Smell_Problem_full" = "survival ~ Measured_Smell_Problem + RIDAGEYR + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

#Supplemental Table 3
models <- list(
  "Sub_Taste_Problem" = "survival ~ Sub_Taste_Problem",
  "Whole_Taste_Problem" = "survival ~ Whole_Taste_Problem",
  "Sub_Taste_Problem_1" = "survival ~ Sub_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Whole_Taste_Problem_1" = "survival ~ Whole_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI",
  "Sub_Taste_Problem_2" = "survival ~ Sub_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Whole_Taste_Problem_2" = "survival ~ Whole_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + DIQ010",
  "Sub_Taste_Problem_full" = "survival ~ Sub_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score",
  "Whole_Taste_Problem_full" = "survival ~ Whole_Taste_Problem + RIDAGEYR  + RIDRETH3 + INDFMPIR + LBXCOT + BMXBMI + MCQ160C + MCQ160E + MCQ220 + PHQ + CERAD_IR + digital_symbol_score"
)

# Initialize a table to store the results
results_table <- data.frame()

# Loop through each model and extract results
for (model_name in names(models)) {
  # Fit the model
  model <- svycoxph(as.formula(models[[model_name]]), design = finalized_table_female)
  
  # Tidy the model output
  tidy_model <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # Extract and format the required values
  tidy_model <- tidy_model %>%
    mutate(
      model = model_name,
      HR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), " - ", round(conf.high, 2), ")"),  # Combine HR and CI
      p_value = round(p.value, 3)
    ) %>%
    select(model, term, HR_CI, p_value)  # Select relevant columns
  
  # Append to the results table
  results_table <- bind_rows(results_table, tidy_model)
}

# View the results table
print(results_table)
write.csv(results_table, "cox_model_results.csv", row.names = FALSE)

#Figure 1
#===================
# Fit survival curve
surv_fit <- svykm(survival ~ Isolated_Whole_Taste_Problem, finalized_table)

# Plot with blue and red lines for each group
plot(surv_fit, 
     main = "Survival Curve by Isolated Psychometric GD (All Participants)",
     xlab = "Follow Up Time (Months)",
     ylab = "Survival Probability",
     col = c("blue", "red"),
     lty = 1,  # Solid lines for both
     ylim = c(0.6, 1))  # Limit y-axis from 0.6 to 1

# Add legend
legend("bottomleft", legend = c("No GD", "Isolated Psychometric GD"),
       col = c("blue", "red"), lty = 1, bty = "n")

all_times <- unlist(lapply(surv_fit, function(group) group$time))
max_time <- max(all_times, na.rm = TRUE)

print(max_time) 

