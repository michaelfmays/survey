
#### Question vectors ####
# Scale aggregate scores
scores <- c("AAS6Score", "ARAS7Score", "ARAS_S5Score")

# Questions of interest
interest <- c(paste("q", c(1, 2, 4, 12:15, 20), sep=""),
              "q21a", "q22b", scores)

# Animal Attitude Scale (AAS)
AAS <- c(paste("q3", letters[1:6], sep=""))

# Animal Research Attitude Scale - Purpose (ARAS-P)
ARAS_P <- c("ARAS_anim_dis", "ARAS_agr", "ARAS_basic_res",
            "ARAS_hum_dis", "ARAS_med_hum", "ARAS_chemicals",
            "ARAS_cosmetics")

# Animal Research Attitude Scale - Species (ARAS-S)
ARAS_S <- c(paste("q6", letters[1:5], sep=""))

# Animal Research Attitude Scale - Species (ARAS-PD)
ARAS_PD <- c(paste("q7", letters[c(1, 3:7)], sep=""),
             paste("q8", letters[c(1, 3:7)], sep=""),
             paste("q9", letters[1:7], sep=""),
             paste("q10", letters[1:7], sep=""),
             paste("q11", letters[1:7], sep=""))

# Other types of questions
importance <- "q1"
knowledge <- c("q2", "q12", "q20")
transl <- "q4"
rules <- c("q12", "q13", "q14", "q15", "q16")
sources <- c(paste("q17", letters[1:7], sep=""),
             paste("q18", letters[1:7], sep=""),
             paste("q19", letters[1:7], sep=""))
personal <- c("q21a", "q22a")
personal_full <- c(paste("q21", letters[1:2], sep=""),
                   paste("q22", letters[1:6], sep=""),
                   paste("q", 23:26, sep=""),
                   paste("q27", letters[1:6], sep=""))

# "Don't know" questions
dont_know <- c(paste("q", 12:16, sep=""), "q20")

# Yes/no questions
yn <- c(paste("q21", letters[1:2], sep=""), 
        paste("q22", letters[1:6], sep=""))

# All questions
all <- c(AAS, ARAS_P, ARAS_S, ARAS_PD, importance,
         knowledge, transl, rules, rules, sources,
         personal_full, scores, yn)

# Save RData object
save(AAS, all, ARAS_P, ARAS_PD, ARAS_S, dont_know,
     importance, interest, knowledge, personal,
     personal_full, rules, sources, transl, yn,
     file = "proj1_vec.RData")

#### All data read and clean ####
# Vectors of variable names to remove
a <- c("acad_level_label", "acad_level_Fres",
       "acad_level_Soph","acad_level_Jun", "acad_level_Sen",
       "dvsn_BioRef", "dvsn_HumanRef","dvsn_PhyRef",
       "dvsn_SocRef", "dvsn_GenRef",
       "s_freshman", "s_sophomore", "s_junior", "s_senior",
       'divisionHumanitiesCat1', 'divisionBiologicalSciCat2' ,
       'divisionPhysicalSciCat3' , 'divisionSocialSciCat4' ,
       'divisionTotal' , 'division1Fraction_P' ,
       'division2Fraction_P' , 'division3Fraction_P' ,
       'division4Fraction_P' , 'division1Fraction_S' ,
       'division2Fraction_S' , 'division3Fraction_S' ,
       'division4Fraction_S' , 'division1_S_F' , 
       'division2_S_F' , 'division3_S_F' , 'division4_S_F')
ar <- c("ARAS_anim_dis", "ARAS_agr", "ARAS_basic_res",
        "ARAS_hum_dis", "ARAS_med_hum", "ARAS_chemicals",
        "ARAS_cosmetics")
non_ord <- c("q18", paste("q", 21:27, sep=""))

# Reading in data
dat_full <- readxl::read_xlsx("DA1_data.xlsx") %>%
  dplyr::select(# Experiment info - useless to me
    -comments, -experiment_group, -`_merge`,
    -page, -submissions, -loaded, -time,
    -survey_type, -cmpl_date,
    -FvsS_D, -SvsF_D,
    -SCREEN_WIDTH, -SCREEN_HEIGHT, -SCREEN_STYLE,
    -contains("String"),
    -starts_with("weight"),
    -starts_with("rake"),
    # Duplicates
    -JobTitle, -Dept_Unit, -exp_group,
    -starts_with("major"),
    # Omnibus
    -one_of(a)) %>%
  mutate(
    
    # Create groups for research question
    ARAS_H_Group = factor(case_when(
      between(ARAS_H_Ave, 1, 2.5)~"low",
      between(ARAS_H_Ave, 2.5, 3.5)~"mid",
      between(ARAS_H_Ave, 3.5, 5)~"high"
    )),
    #ARAS_H_Group = ifelse(is.na(ARAS_H_Group),
    #                      "mid", ARAS_H_Group),
    ARAS_H_Group = fct_relevel(ARAS_H_Group, "mid"),
    
    # Remove &s from certain questions
    #   TODO: Check that this doesn't have unintended
    #   consequences
    across(w(is.character), 
           function(x) str_replace(x, pattern="&",
                                   replacement = "")),
    
    # All of the questions - not the summary statistics/
    #    randomization order/etc
    across(starts_with("q")&!contains("Total")&
             !contains("random")&
             !contains("Score")&!contains("Sum")&
             !contains("Exp")&!starts_with(!!non_ord)&
             !ends_with("_d"), # The _d ones are coded binary.
           #   Maybe worth looking at?
           as.ordered),
    
    # Reordering ordinal factor levels
    #   TODO: Clean up the formatting
    #   TODO: Check that the orders are consisten for each
    q1=fct_relevel(q1, "Not at all important",
                   "A little important", "Somewhat important",
                   "Very important", "Extremely important"),
    across(c(q2, q12), function(x){
      fct_relevel(x, "Nothing", "A little", "Some",
                  "Quite a bit", "A great deal")}),
    across(paste("q3", letters[1:6], sep=""), 
           function(x){
             fct_relevel(x, "Strongly disagree",
                         "Disagree",
                         "Neither agree nor disagree",
                         "Agree", "Strongly agree")}),
    q3a = fct_rev(q3a),
    q3d = fct_rev(q3d),
    q3e = fct_rev(q3e),
    q4=fct_relevel(q4, "Not at all", "A little", "Somewhat",
                   "Quite a bit", "A great deal"),
    across(starts_with("q6")&!ends_with("random"), 
           function(x){
             fct_relevel(x, "Never", "Rarely",
                         "Sometimes", "Usually", "Always")}),
    across(starts_with(paste("q", 7:11, sep=""))&
             !ends_with("random")&!ends_with("_d")&
             !ends_with("Sum"),
           function(x){
             fct_relevel(x, "None", "A little", "Some",
                         "Quite a bit", "A great deal")}),
    q13=fct_relevel(q13, 
                    "...excessive and should be reduced?",
                    "...adequate and should be maintained?",
                    "...not tough enough and should be strengthened?",
                    "Don't know"),
    q13 = fct_rev(q13),
    q14=fct_relevel(q14, "Don't know",
                    "Not at all well", "A little well",
                    "Somewhat well", "Very well", 
                    "Extremely well"),
    q15=fct_relevel(q15, "Don't know", 
                    "Not at all", "A little",
                    "Somewhat", "Very", 
                    "Extremely"),
    q16 = fct_relevel(q16, "Don't know", "Never",
                      "Rarely", "Sometimes",
                      "Usually", "Always"),
    across(starts_with("q17")&
             !ends_with("random")&
             !ends_with("Total"), 
           function(x){
             fct_relevel(x, "Not at all", 
                         "A little", "Some",
                         "Quite a bit", "A great deal")}),
    across(paste("q19", letters[1:7], sep=""),
           function(x){
             fct_relevel(x, "Too little", "Just enough",
                         "Too much")}),
    q20=fct_relevel(q20, "Don't know", 
                    "Not at all", "A little", "Somewhat",
                    "Quite a bit", "A great deal"),
    q12DontKnow_d = factor(ifelse(q12=="Nothing",
                                   "Don't know",
                                   "Substantive Knowledge")),
    # Transform non-ordinal questions to factor,
    #    including randomization order
    across(starts_with("q")&!contains("Total")&
             !contains("Score")&!contains("Sum")&
             !contains("Exp")&starts_with(!!non_ord)&
             !is.ordered, 
           as.factor),
    # Transform ARAS questions (q5_)
    across(matches(!!ar), as.ordered),
    across(matches(!!ar), ~fct_relevel(.x, "Never", "Rarely",
                                       "Sometimes", "Usually",
                                       "Always")),
    ARAS_H_Group = factor(ARAS_H_Group),
    across(w(is.character), as.factor),
    caseid_original = as.character(caseid_original),
    q22b = fct_rev(q22b),
    AAS6Score = 6-AAS6Score
  ) %>%
  rename(S_or_F = data)

dat_small <- dat_full %>%
  dplyr::select(S_or_F, ARAS_H_Group, all, contains("score"))

# Split into two dfs for Students/Faculty
dat_list <- dat_full %>%
  split(., .$S_or_F=="Student")

students <- dat_list[["TRUE"]] %>%
  # Remove NAs in the research question variable
  filter(!is.na(ARAS_H_Group)) %>%
  # Remove faculty-specific variables
  dplyr::select(-starts_with("f_"))

faculty <- dat_list[["FALSE"]] %>%
  # Remove NAs in the research question variable
  filter(!is.na(ARAS_H_Group)) %>%
  # Remove student-specific variables
  dplyr::select(-starts_with("s_"))

final_dat <- list(full=dat_full,
                  small=dat_small,
                  faculty=faculty,
                  students=students,
                  ARAS_qs=ar,
                  interest_qs=interest)

save(dat_full, faculty, students, ar,
     file="proj1.RData")

#### Plot/long data ####
dat_plot2 <- dat_full %>%
  dplyr::select(S_or_F, all, ARAS_H_Group, gender_m_d, division) %>%
  mutate(across(all, function(x) factor(x, ordered = F)),
         across(all, unclass, .names="{.col}_num")
  ) %>%
  rename_with(.cols=all, function(x) paste(x, "_f", sep="")) %>%
  pivot_longer(starts_with(all),
               names_to=c("set", ".value"),
               values_to="Answer",
               names_pattern = "(.+)_(.+)"
  ) %>%
  rename(Question=set,
         Answer=f,
         Ans_num=num) %>%
  na.omit() %>%
  mutate(Question=factor(Question),
         S_or_F = fct_rev(factor(S_or_F)),
         ARAS_H_Group = factor(ARAS_H_Group),
         Answer=fct_rev(factor(Answer)),
         Ans_num=fct_rev(factor(Ans_num)),
         category=case_when(
           Question == "q1" ~ "importance",
           Question == "q2" ~ "arguments",
           Question == "q20" ~ "decisions",
           Question %in% sources ~ "sources",
           Question %in% rules ~ "rules",
           Question %in% ARAS_P ~ "ARAS_P",
           Question %in% ARAS_PD ~ "ARAS_PD",
           Question %in% ARAS_S ~ "ARAS_S",
           Question %in% AAS ~ "AAS",
           Question %in% personal_full ~ "personal",
           Question %in% transl ~ "translat",
           Question %in% scores ~"Score"
         )
  )

dat_plot <- dat_plot2 %>%
  dplyr::select(-gender_m_d, -division)

#### Numeric data #### 
d <- dat_full %>% 
  dplyr::select_if(is.ordered) %>%
  mutate(across(everything(), unclass)) %>%
  rename_with(., ~paste(.x, "_grp", sep=""))

for(j in 1:ncol(d)){
  k <- nrow(unique(d[,j]))
  n_each <- k%/%2
  
  d[,j]<-d %>%
    dplyr::select(j) %>%
    mutate(across(everything(), ~ifelse(.x<n_each&!is.na(.x),
                                        1, ifelse(.x>k-n_each&!is.na(.x), 3,
                                                  ifelse(
                                                    !is.na(.x), 2, NA
                                                  )))))
}

d <- d %>% 
  mutate(across(everything(), as.factor),
         across(is.factor&!is.ordered,
                ~fct_relevel(.x, "2")))

dat_numeric <- dat_full %>%
  dplyr::select(ARAS_H_Group, all, division, gender_m_d,
         acad_level, job_title, 
         contains("dont")&contains("_d"),
         S_or_F,
         all_of(interest)) %>%
  cbind(., d) %>%
  as_tibble()

#sapply(dat_numeric, function(x) sum(is.na(x))) %>% data.frame()

save(dat_numeric, file="proj1_numeric.RData")

#### Tabulations ####

dat_tab_gend <- dat_plot2 %>%
  filter(Question%notin%c("q23", "q24")&
           Answer%notin%c("Nothing", "Don't know")) %>%
  group_by(ARAS_H_Group, S_or_F, gender_m_d, Question) %>%
  mutate(Ans_num = as.numeric(as.character(Ans_num)),
         Ans_num = ifelse(Question %in% scores,
                          as.numeric(as.character(Answer)),
                          Ans_num)) %>%
  mutate(Ans_num = ifelse((Question%in%dont_know&
                             Question!="q12")|
                            Question%in%yn,
                          Ans_num-1,
                          Ans_num)) %>%
  summarize(mean=mean(Ans_num), sd=sd(Ans_num),
            .groups="drop") %>%
  mutate(mean=ifelse(Question%in%yn,
                     mean*100, mean)) %>%
  mutate(mean = paste(round(mean, 2),
                      " (", round(sd,2), ")",
                      sep="")) %>%
  select(-sd) %>%
  rename(term = gender_m_d)

dat_tab_div <- dat_plot2 %>%
  filter(Question%notin%c("q23", "q24")&
           Answer%notin%c("Nothing", "Don't know")) %>%
  group_by(ARAS_H_Group, S_or_F, division, Question) %>%
  mutate(Ans_num = as.numeric(as.character(Ans_num)),
         Ans_num = ifelse(Question %in% scores,
                          as.numeric(as.character(Answer)),
                          Ans_num)) %>%
  mutate(Ans_num = ifelse((Question%in%dont_know&
                             Question!="q12")|
                            Question%in%yn,
                          Ans_num-1,
                          Ans_num)) %>%
  summarize(mean=mean(Ans_num), sd=sd(Ans_num),
            .groups="drop") %>%
  mutate(mean=ifelse(Question%in%yn,
                     mean*100, mean)) %>%
  mutate(mean = paste(round(mean, 2),
                      " (", round(sd,2), ")",
                      sep="")) %>%
  select(-sd) %>%
  rename(term = division)

dat_tab_grp <- dat_plot2 %>%
  filter(Question%notin%c("q23", "q24")&
           Answer%notin%c("Nothing", "Don't know")) %>%
  group_by(ARAS_H_Group, S_or_F, Question) %>%
  mutate(Ans_num = as.numeric(as.character(Ans_num)),
         Ans_num = ifelse(Question %in% scores,
                          as.numeric(as.character(Answer)),
                          Ans_num)) %>%
  mutate(Ans_num = ifelse((Question%in%dont_know&
                             Question!="q12")|
                            Question%in%yn,
                          Ans_num-1,
                          Ans_num)) %>%
  summarize(mean=mean(Ans_num), sd=sd(Ans_num),
            .groups="drop") %>%
  mutate(mean=ifelse(Question%in%yn,
                     mean*100, mean)) %>%
  mutate(mean = paste(round(mean, 2),
                      " (", round(sd,2), ")",
                      sep="")) %>%
  select(-sd) %>%
  mutate(term=ARAS_H_Group)

dat_tab <- bind_rows(dat_tab_gend, dat_tab_div) %>%
  mutate(Question=as.character(Question))

dat_tab_wide <- dat_tab %>%
  mutate(ARAS_H_Group = fct_relevel(ARAS_H_Group,
                                    "low", "mid", "high")) %>%
  arrange(ARAS_H_Group, Question) %>%
  pivot_wider(id_cols=c(term, Question),
              names_from=c(S_or_F, ARAS_H_Group),
              values_from=mean) %>%
  select(term, Question, starts_with("S"), everything())

dont_know_tab <- dat_full %>%
  select(S_or_F, ARAS_H_Group,
         contains("DontKnow_d")&
           !contains("Exp")) %>%
  mutate(across(contains("DontKnow_d"),
                as.character),
         ARAS_H_Group=fct_relevel(ARAS_H_Group, "low",
                                  "mid", "high")) %>%
  pivot_longer(cols=contains("DontKnow_d"),
               names_to="Question", values_to="Answer") %>%
  drop_na() %>%
  group_by(ARAS_H_Group, S_or_F, Question) %>%
  summarize(x=sum(Answer=="Don't Know/Nothing")+
              sum(Answer=="Don't know"),
            n=n()) %>%
  group_by(S_or_F, Question) %>%
  summarize(summary=list(broom::tidy(prop.test(x=x, n=n))),
            .groups="drop") %>%
  unnest(summary) %>%
  rename(low=estimate1, mid=estimate2, high=estimate3,
         p=p.value) %>%
  mutate(sig = case_when(
    p<0.001 ~ "***",
    p<0.01 ~"**",
    p<0.05 ~ "*",
    TRUE ~ " ns")) %>%
  mutate(stat_sig=paste(round(statistic, 2), sig, sep="")) %>%
  select(-p, -parameter, -method,
         -sig, -alternative, -statistic) %>%
  mutate(Question = substr(Question, 1, 3)) %>%
  pivot_wider(id_cols="Question",
              names_from=c("S_or_F"),
              values_from=c("low", "mid", "high",
                            "stat_sig")) %>%
  select(Question, paste(c("low", "mid", "high"),
                         "_Student",  sep=""),
         stat_sig_Student,
         paste(c("low", "mid", "high"),
               "_Faculty",  sep=""),
         stat_sig_Faculty) %>%
  add_column(., term=rep("Don't Know (%)",
                         nrow(.)),
             .before="Question") %>%
  select(term, low_Student:stat_sig_Faculty, everything()) %>%
  mutate(across(is.numeric, ~as.character(100*round(.x, 3))))

#### Look-up df for flextable ####
short_desc <- c("Importance",
                "Knowledge of Facts and Arguments",
                "Translatability",
                "Knowledge of Rules and Regulations",
                "Rule Sufficiency",
                "Enforcement of Laws and Guidelines",
                "Review of Research Proposals",
                "Ability to Make Informed Decisions",
                "Worked on Animal Research Project",
                "Member of Animal Activist Organization", 
                "Mean of Answers to AAS Questions",
                "Mean of Answers to ARAS-Purpose Questions",
                "Mean of Answers to ARAS-S Questions")
long_desc <- c("percieved importance of animal research.",
  "self-reported knowledge of arguments for and against animal research.",
  "percieved translatability of animal research to humans.",
  "self-reported knowledge of animal research rules and regulations.",
  "percieved sufficiency of animal research rules and regulations.",
  "perception that animal research laws and guidelines are well-enforced.",
  "perception that animal research proposals are thoroughly reviewed.",
  "self-reported knowledge to make informed decisions. Excludes those answering \"Don't know\".",
  "0=No, 1=Yes",
  "0=Yes, 1=No", 
  "AAS Score",
  "ARAS-P Score",
  "ARAS-S Score")

long_desc[1:8] <- paste("Higher numbers indicate greater ",
                        long_desc[1:8], sep="")

cat_df <- dat_plot2 %>% 
  dplyr::select(category, Question) %>%
  filter(Question %in% interest) %>%
  distinct() %>%
  arrange(factor(Question, levels=interest)) %>%
  cbind(.,
        short_desc=short_desc,
        long_desc=long_desc) %>%
  mutate(category=simple_cap(category),
         scale=case_when(
           Question%in%c("q21a", "q22b") ~ "Yes/No",
           Question == "q13" ~ "1-3",
           Question == "q12" ~ "2-5",
           TRUE ~ "1-5"
         ))

save(dat_plot2, dat_plot, cat_df, file="proj1_long.RData")
