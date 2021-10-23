w <- tidyselect::vars_select_helpers$where

`%notin%` <- Negate(`%in%`)

simple_cap <- function(x) {
  s <- x
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="")
}

l <- function(x){
  y <- paste("Question ", substring(x, 2), sep="")
  y
}

l_score <- function(x){
  x = case_when(substr(x, 1, 5)=="AAS6S"~"AAS",
                substr(x, 1, 5)=="ARAS7"~"ARAS-P",
                substr(x, 1, 5)=="ARAS_"~"ARAS-S")
  
  y <- paste(x, "  Score", sep="")
  y
}

insert_screenshot = function(x, png_file) {
  htmltools::save_html(htmltools_value(x), 'temp.html')
  res = webshot::webshot('temp.html',png_file, zoom=3,
                         vwidth=896, vheight = 606,
                         delay = .5, expand=10)
  unlink('temp.html')
  res
}

dk_tests_1 <- function(data, va){
  #### Group sizes ####
  grp_sizes <- dat_full %>% 
    group_by(S_or_F, ARAS_H_Group) %>% 
    summarize(n=n())
  
  #if(min(grp_sizes$n)<10){
  #grp_sizes <- dat_full %>% 
  #  group_by(S_or_F, ARAS_H_Group) %>% 
  #  summarize(n=n())
  #}
  
  n_unique <- dat_numeric %>%
    dplyr::select(va) %>% 
    na.omit() %>% 
    unique() %>% 
    nrow()
  
  dk_tests_one <- data %>%
    group_by(S_or_F, ARAS_H_Group) %>%
    dplyr::select(paste(va, "DontKnow_d", sep="")) %>%
    summarize(n()) %>%
    drop_na() %>%
    inner_join(., grp_sizes) %>% 
    rename(k=`n()`) %>%
    rowwise() %>%
    mutate(tst=list(broom::tidy(prop.test(x=k, n=n,
                                          p=1/n_unique)))) %>%
    unnest(tst)
  
  dk_tests_one
}

dk_tests_2 <- function(data, va){
  #### Group sizes ####
  grp_sizes <- dat_full %>% 
    group_by(S_or_F, ARAS_H_Group) %>% 
    summarize(n=n())
  
  #if(min(grp_sizes$n)<10){
  #grp_sizes <- dat_full %>% 
  #  group_by(S_or_F, ARAS_H_Group) %>% 
  #  summarize(n=n())
  #}
  
  dk_tests_two_a <- data %>%
    group_by(S_or_F, ARAS_H_Group) %>%
    dplyr::select(paste(va, "DontKnow_d", sep="")) %>%
    summarize(n()) %>%
    inner_join(., grp_sizes) %>% 
    rename(k=`n()`) %>%
    group_by(ARAS_H_Group) %>%
    #filter(ARAS_H_Group!="other") %>%
    #dplyr::select(-n.S_or_F) %>%
    drop_na() %>%
    mutate(tst=list(broom::tidy(prop.test(x=k, n=n)))) %>%
    unnest(tst) %>% 
    rstatix::adjust_pvalue() %>%
    mutate(grp="Didn't know",
           test_set="between_S_F")
  
  dk_tests_two_b <- data %>%
    group_by(S_or_F, ARAS_H_Group) %>%
    dplyr::select(paste(va, "DontKnow_d", sep="")) %>%
    summarize(n()) %>%
    inner_join(., grp_sizes) %>% 
    rename(k=`n()`) %>%
    group_by(S_or_F) %>%
    #filter(ARAS_H_Group!="other") %>%
    #dplyr::select(-n.S_or_F) %>%
    drop_na() %>%
    mutate(tst=list(broom::tidy(prop.test(x=k, n=n)))) %>%
    unnest(tst) %>% 
    rstatix::adjust_pvalue() %>%
    mutate(grp="Didn't know",
           test_set="between_ARAS")
  
  bind_rows(dk_tests_two_a, dk_tests_two_b)
}

anova_test <- function(data, va, tests){
  
  test_list <- list()

  f <- formula(paste(va, "~ARAS_H_Group", sep=""))
  
  for(t in 1:length(tests)){
    test_list[[t]] <- data %>%
      dplyr::select(va, v) %>%
      group_by(S_or_F, str_glue("{", tests[t],"}")) %>% #View()
      do(anova(aov(formula=f, data=.))) %>%
      drop_na() %>%
      ungroup() %>%
      mutate(grp_var=tests[t]) %>%
      rename(grping="str_glue(\"{\", tests[t], \"}\")",
             statistic="F value", p="Pr(>F)") %>%
      dplyr::select(S_or_F, grping, Df, grp_var, statistic, p)
  }
  
  return(bind_rows(test_list))
}

kw_test <- function(data, va, tests, names, know_grp){
  
  test_list <- list()
  
  resp <- {if(va %in% names){paste("{", va, "_grp}", sep="")}else{
    paste("{", va, "}", sep="")}}
  
  for(t in 1:length(tests)){
    test_list[[t]] <- data %>%
      dplyr::select(everything(), va[va %notin% ARAS_PD]) %>%
      group_by(S_or_F, str_glue("{", tests[t],"}")) %>%
      rstatix::kruskal_test(str_glue(resp)~ARAS_H_Group) %>% 
      rstatix::adjust_pvalue() %>%
      mutate(.y.=paste(va, "_grp", sep=""),
             know_grp=as.character(know_grp),
             grp_var=tests[t]) %>%
      rename(y=.y.,
             grping="str_glue(\"{\", tests[t], \"}\")")
  }
  
  return(bind_rows(test_list))
  
}

model_func <- function(data, va, names, stu_fac, dont_know_vec, is_dk){
  
  f <- if(va %notin% dont_know_vec | is_dk==F){
    formula(paste(va, "~.", sep=""))
         } else {
    formula("ARAS_H_Group~gender_m_d+division", sep="")
         }
  
  if(va%notin% scores){
    mod <- data %>%
    {if(is_dk==TRUE | va %in% names){
      dplyr::select(., -paste(va, "_grp", sep=""))}else{.}} %>%
    #{if(is_dk==TRUE | va %in% names){dplyr::select(., -va)}else{.}} %>%
    {if(stu_fac=="student"){dplyr::select(., -acad_level)}
      else{dplyr::select(., -job_title)}} %>%
    {if(va %in% dont_know_vec){
      dplyr::select(., -paste(va, "DontKnow_d", sep=""))}else{.}} %>%
    drop_na() %>%
    {if(va %notin% yn){
      ordinal::clm(formula=f,data=., link="logit")}
      else{glm(formula=f, data=., family="binomial")}
      } %>%
    list(lm=.,
         summary=tibble(broom::tidy(.),
                        OR=exp(coef(.))
                        ),
         #OR=tibble(exp(coef(.))),
         OR_CI=exp(confint(profile(.))),
         pseudo_r2=pscl::pR2(.),
         q=va)
  } else {
    mod <- data %>%
      {if(stu_fac=="student"){dplyr::select(., -acad_level)}
        else{dplyr::select(., -job_title)}} %>%
      lm(formula=f, data=.) %>%
      list(lm=.,
           summary=tibble(broom::tidy(.)),
           OR_CI=confint(.),
           q=va)
    
    mod$summary <- mod$summary %>%
      mutate(OR=estimate)
  }
  
  colnames(mod$OR_CI) <- c("L", "U")
  
  mod
}

count_func <- function(data, va, names, dont_know_vec){
  data %>% 
    group_by(S_or_F, ARAS_H_Group) %>%
    {if(va %in% dont_know_vec){
      group_by(., str_glue("{", va,"DontKnow_d}"), .add=T)}else{.}} %>%
    {if(va %in% names){group_by(., str_glue("{", va,"_grp}"),
                                .add=T)}
      else{group_by(., str_glue("{", va,"}"),
                    .add=T)}} %>%
    summarize(n=n()) %>%
    {if(va %in% names){rename(., q_grp="str_glue(\"{\", va, \"_grp}\")")}
      else(rename(., q_grp="str_glue(\"{\", va, \"}\")"))} %>%
    {if(va %in% dont_know_vec){
      rename(., dk_grp="str_glue(\"{\", va, \"DontKnow_d}\")")}else{.}} %>%
    mutate(q=va)
}

test_func <- function(list){
  
  z_func <- function(a, b, c, d){
    z <- (a-b)/sqrt((a^2)+(b^2))
    p_z <- 2*pnorm(abs(z), lower.tail = F)
    
    return(tibble(z=z, p=p_z))
  }
  
  if(list$q %in% dont_know){
    # Don't know
    s_dk <- list$dk$student$summary
    f_dk <- list$dk$faculty$summary
    
    # Know
    s_know <- list$know$student$summary
    f_know <- list$know$faculty$summary
    
    # Terms
    terms <- s_dk$term
    terms_2 <- s_know$term
    
    # Don't know
    b_1 <- s_dk$estimate
    b_2 <- f_dk$estimate
    se_b_1 <- s_dk$std.error
    se_b_2 <- f_dk$estimate
    
    # Z - don't know
    z_dk <- z_func(b_1, b_2, se_b_1, se_b_2)
    
    # Know
    b_3 <- s_know$estimate
    b_4 <- f_know$estimate
    se_b_3 <- s_know$std.error
    se_b_4 <- f_know$estimate
    
    # Z - know
    z_know <- z_func(b_3, b_4, se_b_3, se_b_4)
    
    ret <- list(dk=tibble(terms, z_dk),
                know=tibble(terms=terms_2, z_know))
    
  } else {
    # Both
    s_df <- list$student$summary
    f_df <- list$faculty$summary
    
    # Terms
    terms <- s_df$term
    
    # Ests
    b_1 <- s_df$estimate
    b_2 <- f_df$estimate
    se_b_1 <- s_df$std.error
    se_b_2 <- f_df$std.error
    
    # Z - both
    z_both <- z_func(b_1, b_2, se_b_1, se_b_2)
    
    ret <- tibble(terms, z_both)
    
  }
  
  return(ret)
  
}

quest_func <- function(question){ 
  
  # Necessary
  `%notin%` <- Negate(`%in%`)
  
  #### Define vectors I'll need ####
  # All ordered variables -- these have _grp variables
  names <- dat_full %>% 
    dplyr::select_if(is.ordered) %>%
    names()
  
  # Variables to always include in data frame
  v <- c("ARAS_H_Group", "gender_m_d", 
         "division", "acad_level", "job_title",
         "S_or_F")
  
  # Variables to group_by for tests
  tests <- c("gender_m_d", "division",
             "acad_level", "job_title")
  
  ##### Define variable of interest ####
  va <- question
  
  #### Define df ####
  df <- dat_numeric %>% dplyr::select(v, va,
        contains(va)&contains("_grp")&!contains("Sub")&!contains("_md"),
        contains(va)&contains("dont")&contains("_d")&!contains("exp")
  ) %>%
    mutate(gender_m_d = fct_relevel(gender_m_d, "male"))
  
  if(va=="q1"){
    df <- dat_numeric %>% dplyr::select(v, va, "q1_grp"
    ) %>%
      mutate(gender_m_d = fct_relevel(gender_m_d, "male"))
  }
  if(va=="q2"){
    df <- dat_numeric %>% dplyr::select(v, va, "q2_grp") %>%
      mutate(gender_m_d = fct_relevel(gender_m_d, "male"))
  }
  if(va%in%scores){
    df <- dat_numeric %>% dplyr::select(v, va) %>%
      mutate(gender_m_d = fct_relevel(gender_m_d, "male"))
  }
  
  #### The actual function #####
  # TODO: Add in check for Q12: "Nothing" response="Don't know"
  if(va %in% dont_know) {
    #### Didn't know ####
    dk_df <- df %>% 
        filter(str_glue(paste("{", va, "DontKnow_d}",
                            sep=""))!="Substantive Knowledge")
    ### Hypo tests
    dk_tests_one <- dk_tests_1(dk_df, va)
    dk_tests_two <- dk_tests_2(dk_df, va)
    
    ### Modeling
    # Split data
    split_dk_df <- dk_df %>%
      split(., .$S_or_F=="Student")
    s_dk_df <- split_dk_df[["TRUE"]] %>%
      dplyr::select(-starts_with("f_"), -job_title, -S_or_F)
    f_dk_df <- split_dk_df[["FALSE"]] %>%
      dplyr::select(-starts_with("s_"), -acad_level, -S_or_F)
    
    # Make models
    s_dk_list <- model_func(s_dk_df, va, names,
                            "student", dont_know, is_dk=T)
    f_dk_list <- model_func(f_dk_df, va, names,
                            "faculty", dont_know, is_dk=T)
    
    #### Did know ####
    know_df <- df %>% 
      filter(str_glue(paste("{", va, "DontKnow_d}",
                            sep=""))=="Substantive Knowledge")
    
    ### Hypo tests
    know_tests <- kw_test(know_df, va, tests, names, "Know")
    
    ### Modeling
    # Split data
    know_df_split <- know_df %>%
      split(., .$S_or_F=="Student")
    s_know_df <- know_df_split[["TRUE"]] %>%
      dplyr::select(-starts_with("f_"), -job_title, -S_or_F)
    f_know_df <- know_df_split[["FALSE"]] %>%
      dplyr::select(-starts_with("s_"), -acad_level, -S_or_F)
    
    # Make model lists
    s_know_list <- model_func(s_know_df, va, names,
                              "student", dont_know, is_dk=F)
    f_know_list <- model_func(f_know_df, va, names,
                              "faculty", dont_know, is_dk=F)
    
    ### Counts
    count <- count_func(df, va, names, dont_know)
    
    ### Return list
    li <- list(dk=list(
      tests_one_samp=dk_tests_one, 
      test_two_samp=dk_tests_two,
      student=s_dk_list,
      faculty=f_dk_list
    ),
    know=list(
      tests=know_tests, 
      student=s_know_list,
      faculty=f_know_list
    ),
    counts=count,
    q=va)
    
    ### Post-hoc tests
    li <- append(li, list(ztest=test_func(li)))
    
    ### Return
    return(li)
  
    #### Scores ####
  } else if(va %in% scores){
    ### Hypo tests
    score_tests <- anova_test(df, va, tests)
    
    ### Modeling
    # Split data
    score_df_split <- df %>%
      split(., .$S_or_F=="Student")
    s_score_df <- score_df_split[["TRUE"]] %>%
      dplyr::select(-starts_with("f_"), -job_title, -S_or_F) %>%
      mutate(acad_level=as.factor(acad_level))
    f_score_df <- score_df_split[["FALSE"]] %>%
      dplyr::select(-starts_with("s_"), -acad_level, -S_or_F)
    
    # Make models
    s_list <- model_func(s_score_df, va, names,
                         "student", dont_know, is_dk=F)
    f_list <- model_func(f_score_df, va, names,
                         "faculty", dont_know, is_dk=F)
    
    ### Counts
    score_count <- count_func(df, va, names, dont_know)
    
    ### Return list
    li <- list(
      tests=score_tests, 
      student=s_list,
      faculty=f_list,
      counts=score_count,
      q=va
    )
    
    return(li)
    
  } else {
    ### Modeling
    # Split data
    both_df_split <- df %>%
      split(., .$S_or_F=="Student")
    s_both_df <- both_df_split[["TRUE"]] %>%
      dplyr::select(-starts_with("f_"), -job_title, -S_or_F) %>%
      mutate(acad_level=as.factor(acad_level))
    f_both_df <- both_df_split[["FALSE"]] %>%
      dplyr::select(-starts_with("s_"), -acad_level, -S_or_F)
    
    # Make models
    s_list <- model_func(s_both_df, va, names,
                         "student", dont_know, is_dk=F)
    f_list <- model_func(f_both_df, va, names,
                         "faculty", dont_know, is_dk=F)
    
    ### Hypo tests
    both_tests <- kw_test(df, va, tests, names, "Both/NA")
    
    ### Counts
    count <- count_func(df, va, names, dont_know)
    
    ### Return list
    li <- list(
      tests=both_tests, 
      student=s_list,
      faculty=f_list,
      counts=count,
      q=va
    )
    
    # Post-hoc tests
    li <- append(li, list(ztest=test_func(li)))
    
    ### Return
    return(li)
  }
}

plot_func <- function(subset, data=dat_plot){
  simple_cap <- function(x) {
    s <- x
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="")
  }
  
  l <- function(x){
    y <- paste("Question ", substring(x, 2), sep="")
    y
  }
  
  g <- function(x){y <- paste("Group ", simple_cap(x), sep=""); y}
  
  data %>%
    filter(Question %in% subset) %>%
    arrange(Question, Ans_num) %>%
    group_by(Ans_num) %>%
    mutate(a=factor(paste0(unique(Answer),
                           collapse="/\n")),
           Question=factor(Question,
                           levels=subset)
    ) %>%
    ungroup() %>% 
    ggplot(aes(x=S_or_F,
               fill=a)) +
    geom_bar(position="fill") +
    facet_grid(ARAS_H_Group~Question,
               labeller=labeller(Question=l,
                                 ARAS_H_Group=g)) +
    scale_fill_brewer(direction=-1,
                      palette=7,
                      name="Answer") +
    scale_x_discrete(name=NULL) +
    theme_bw() +
    labs(y=NULL)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
}

or_sig <- function(list){
  q <- list$q
  
  if(list$q %in% dont_know){
    s_dk <- list$dk$student$summary
    f_dk <- list$dk$faculty$summary
    name_dk <- colnames(list$dk$student$lm$model)
    
    s_know <- list$know$student$summary
    f_know <- list$know$faculty$summary
    name_know <- colnames(list$know$student$lm$model)
  }else{
    s <- list$student$summary
    f <- list$faculty$summary
    name <- colnames(list$student$lm$model)
  }
  
  if(q %in% scores){
    vs_div <- "Coefficient vs. Bio (SE): "
    vs_gen <- "Coefficient vs. Male (SE): "
  } else {
    vs_div <- "OR vs. Bio (SE): "
    vs_gen <- "OR vs. Male (SE): "
  }
  
  sum_func <- function(summary, group, know){
    
    if(know=="dk"){
      name <- name_dk
    } else if(know=="know"){
      name <- name_know
    }
    
    d <- summary %>%
      filter(str_detect(term, fixed("|"), negate = T)) %>%
      filter(str_detect(term, coll("(Intercept)"),
                        negate = T)) %>%
      dplyr::select(term, OR, p.value, std.error) %>%
      mutate(sig = case_when(
        p.value<0.001/n() ~ "***",
        p.value<0.01/n() ~"**",
        p.value<0.05/n() ~ "*",
        TRUE ~ " ns"
      ),
      vs=case_when(
        substr(term, 1, 3) == "div" ~ vs_div,
        substr(term, 1, 3) == "gen" ~ vs_gen,
        TRUE~""
      ),
      OR_sig=paste(vs, round(OR, 2), sig,
                   " (", round(std.error, 2), ") ",
                   sep="")) %>%
      separate(term, c("v", "ter"), 
               paste(str_flatten(name, fixed("|")),
                     "|^(.*)$", sep=""),
               remove=F) %>%
      dplyr::select(-v) %>%
      separate(., term, c("var", "t"),
               paste("^(.*)$.|",
                     str_flatten(.$ter,
                                 fixed("|")), sep=""),
               remove=F) %>%
      dplyr::select(-t, -term, -OR, -p.value, -sig, -vs) %>%
      rename(variable=var,
             term=ter) %>%
      mutate(S_or_F=group,
             Question=list$q) %>%
      {if(know!="NA"){mutate(., know=know)}else{.}}
    
    d
  }
  
  if(list$q %in% dont_know){
    return(list(dk=list(student=sum_func(s_dk, "student", "dk"),
                        faculty=sum_func(f_dk, "faculty", "dk")),
                know=list(student=sum_func(s_know, "student", "know"),
                          faculty=sum_func(f_know, "faculty", "know"))))
  }else{
    return(list(student=sum_func(s, "student", "NA"),
                faculty=sum_func(f, "faculty", "NA")))
  }
  
}

or_sig_cleanup <- function(or_sig_output, va){
  or_sig_output %>%
    {if(va %in% dont_know){
      unlist(., recursive=F)}else{.}} %>%
    bind_rows() %>% 
    drop_na() %>% 
    add_row(tibble(variable="ARAS_H_Group",
                   term="mid", OR_sig="---",
                   S_or_F=c("student", "faculty"),
                   Question=rep(unique(.$Question), 2))) %>%
    {if(va %in% dont_know){
      add_row(., tibble(variable="ARAS_H_Group",
                        term="mid", OR_sig="---",
                        S_or_F=c("student", "faculty"),
                        Question=rep(unique(.$Question), 2)))}else{.}} %>%
    {if(va %in% dont_know){
      mutate(., know=ifelse(term=="mid",
                            rep(c("dk", "know"), times=2),
                            know))}else{.}} %>%
    {if(va %in% dont_know){split(., .$know=="dk")}else{.}}
}

test_sig_func <- function(test_df, dk=T, samp="one", yesno=F){
  test_df %>%
    {if(dk==F)
    {filter(., grping!="NA",
            grp_var%notin%c("acad_level",
                            "job_title"))}else{.}} %>% 
    {if(dk==T & samp=="one"){
      rename(., grping = ARAS_H_Group, p=p.value)}else{.}} %>%
    {if(dk==T & samp=="two"){
      rename(., grping = test_set, p=p.value)}else{.}} %>%
    dplyr::select(S_or_F, grping, statistic, p) %>%
    {if(yesno==T){
      mutate(., sig = case_when(
        p<0.001 ~ "***",
        p<0.01 ~"**",
        p<0.05 ~ "*",
        TRUE ~ " ns"))
    }else{mutate(., sig = case_when(
                  p<0.001/n() ~ "***",
                  p<0.01/n() ~"**",
                  p<0.05/n() ~ "*",
                  TRUE ~ " ns"))}} %>%
    mutate(stat_sig=paste(round(statistic, 2), sig, sep="")) %>%
    dplyr::select(-statistic, -p, -sig) %>%
    rename(term=grping) %>% 
    distinct()
}

linear_test_postprocessing_func <- function(test_df, name, va){
  test_df %>%
    filter(str_detect(terms, fixed("|"), negate = T)) %>%
    filter(str_detect(terms, coll("Intercept"),
                      negate = T)) %>%
    separate(terms, c("v", "ter"), 
             paste(str_flatten(name, fixed("|")),
                   "|^(.*)$", sep=""),
             remove=F) %>%
    dplyr::select(-v) %>%
    separate(., terms, c("var", "t"),
             paste("^(.*)$.|", str_flatten(.$ter,
                                           fixed("|")), sep=""),
             remove=F) %>%
    dplyr::select(-terms, -t) %>%
    rename(term=ter, variable=var) %>%
    mutate(term=as.character(term),
           term = case_when(
             term=="male"~"Male",
             term=="female"~"Female",
             TRUE~term
           ), sig = case_when(
             p<0.001/n() ~ "***",
             p<0.01/n() ~"**",
             p<0.05/n() ~ "*",
             TRUE ~ " ns"
           ),
           z=paste(round(z, 2), sig, sep="")) %>%
    dplyr::select(-p, -sig)
}

tab_make_func <- function(oa, te, va){
  dat_tab %>% 
    drop_na() %>%
    filter(Question %in% interest) %>%
    {if(va %notin% dont_know){
      full_join(., oa, by=c("S_or_F", "term", "Question"))
    } else {
      full_join(., oa[["FALSE"]], by=c("S_or_F", "term",
                                       "Question"))}} %>% 
    full_join(dat_tab_grp, by=c("S_or_F", "Question",
                                "term", "ARAS_H_Group",
                                "mean")) %>%
    filter(Question==va) %>%
    mutate(variable = case_when(
      substr(term, 1, 3) %in% c("Bio", "Phy",
                                "Soc", "Art", "Hum") ~
        "division",
      substr(term, 1, 3) %in% c("fem", "mal") ~ "gender_m_d",
      substr(term, 1, 3) %in% c("hig", "mid", "low") ~
        "ARAS_H_Group"
    )) %>%
    arrange(variable, term, S_or_F) %>%
    mutate(is_or=ifelse(!is.na(OR_sig), 1, 0),
           S_or_F = case_when(
             S_or_F%in%c("student", "Student")~"Student",
             S_or_F%in%c("faculty", "Faculty")~"Faculty"
           ),
           ARAS_H_Group = case_when(
             term %in% c("low", "mid", "high")&
               is.na(ARAS_H_Group) ~ term,
             TRUE ~ as.character(ARAS_H_Group)
           ),
           term=case_when(
             term %in% c("low", "mid", "high")& 
               !is.na(OR_sig)~"ARAS_H_Group",
             TRUE ~ as.character(term)
           )) %>% 
    full_join(te, by=c("S_or_F", "term")) %>%
    {if(va%in%dont_know){
      dplyr::select(., -know)}else{.}} %>%
    distinct() %>%
    pivot_wider(id_cols=c(S_or_F, variable,
                          term, Question, is_or,
                          stat_sig),
                names_from=c(ARAS_H_Group),
                values_from=c(mean, OR_sig)) %>%
    {(if("OR_sig_NA" %in% colnames(.)){
      mutate(., across(c("mean_mid", "mean_low",
                         "mean_high"),
                       ~ifelse(!is.na(OR_sig_NA),
                               OR_sig_NA, .x)))}
      else{.})} %>% 
    mutate(across("mean_mid", ~ifelse(!is.na(OR_sig_mid),
                                      OR_sig_mid, .x)),
           across("mean_low", ~ifelse(!is.na(OR_sig_low),
                                      OR_sig_low, .x)),
           across("mean_high", ~ifelse(!is.na(OR_sig_high),
                                       OR_sig_high, .x))) %>%
    group_by(S_or_F, is_or) %>%
    fill(c("mean_mid", "mean_low", "mean_high"),
         .direction="up") %>% 
    mutate(term = ifelse(term=="high", "ARAS_H_Group", term)) %>%
    filter(term%notin%c("low", "mid")) %>% 
    pivot_wider(id_cols=c(variable, term, Question, is_or),
                names_from=c(S_or_F),
                values_from=c(mean_mid, mean_low, mean_high,
                              stat_sig)) %>%
    rename_with(.cols=starts_with("mean"),
                ~gsub("mean_", "", .x)) %>% 
    dplyr::select(term, is_or, Question,
           paste(c("low", "mid", "high"),"_Student",  sep=""),
           stat_sig_Student,
           paste(c("low", "mid", "high"),"_Faculty",  sep=""),
           stat_sig_Faculty,
           starts_with("F"), everything(), -ends_with("NA")) %>%
    arrange(variable, term, is_or) %>% 
    group_by(is_or) %>%
    distinct() %>%
    ungroup() %>%
    mutate(term=as.character(term),
           term = case_when(
             term=="male"~"Male",
             term=="female"~"Female",
             term=="ARAS_H_Group"~"ARAS-PD Group",
             TRUE~term
           ),
           stat_sig_Student=ifelse(variable=="ARAS_H_Group", NA,
                                   stat_sig_Student)) 
}

tab_form_func <- function(list){
  
  va <- list$q
  
  if(va %notin% dont_know){
    tests <- list$tests
  } else {
    dk_tests_1 <- list$dk$tests_one_samp %>% 
      mutate(grp_var="one")
    dk_tests_2 <- list$dk$test_two_samp %>% 
      mutate(grp_var="two")
    tests <- list$know$tests
  }
  
  if(va %in% dont_know){
    name_dk <- colnames(list$dk$student$lm$model)
    name <- colnames(list$know$student$lm$model)
  } else {
    name <- colnames(list$student$lm$model)
  }
  
  oa <- or_sig(list) %>% 
    or_sig_cleanup(va)
  
  is_yn <- ifelse(va %in% yn, T, F)
  
  te <- test_sig_func(tests, dk=F, yesno=is_yn)
  
  if(va %in% dont_know){
    te_dk_1 <- test_sig_func(dk_tests_1)
    te_dk_2 <- test_sig_func(dk_tests_2)
  }
  
  tab <- tab_make_func(oa, te, va)
  
  u <- unique(c(tab$term[c(2,1)],
                tab$term[c(12, 10)],
                tab$term[3:9]))
  
  tes <- test_func(list) %>%
    {if(va %in% dont_know){.$know}else{.}} %>%
    linear_test_postprocessing_func(name, va)
  
  tab <- full_join(tab, tes, by=c("variable", "term")) %>%
    mutate(z = ifelse(term%in%c("Male", "ARAS-PD Group"),
                      " ", z))
  
  tab <- tab %>%
    mutate(z = ifelse(term %in%c("high", "low"),
                      paste(simple_cap(term),
                            ": ", z, sep=""), z)) %>% 
    .[c(rownames(.)[-c((nrow(.)-1),nrow(.))],
        nrow(.), (nrow(.)-1)), ]
  
  zs <- tab %>% filter(term %in% c("high", "low")) %>% dplyr::select(z)
  
  tab[1:2, "z"] <- zs
  
  tab <- tab %>% filter(term %notin% c("high", "low"))
  
  tab <- tab %>%
    arrange(factor(term, levels=u))
  
  if(va %in% dont_know){
    tab$mid_Student[2] <- "---"
  }
  
  tab <- tab %>%
    mutate(across(everything(), ~ifelse(is.na(.x), "", .x)))
  
  return(tab)
  
}

custom_flextab <- function(list, borders=F){
  tab <- tab_form_func(list) #%>% 
    #mutate(across(everything(),
       #~ifelse(.x=="0 (0)", "NA", .)))
  
  qu <- list$q
  
  if(qu %in% dont_know){
    dk_tab <- dont_know_tab %>%
      filter(Question==qu) %>%
      mutate(Question="")
  }
  
  short_desc <- cat_df %>%
    filter(Question==qu) %>%
    dplyr::select(short_desc) %>% 
    unlist() %>% unname()
  
  long_desc <- cat_df %>%
    filter(Question==qu) %>%
    dplyr::select(long_desc) %>% 
    unlist() %>% unname()
  
  scale <- cat_df %>%
    filter(Question==qu) %>%
    dplyr::select(scale) %>% 
    unlist() %>% unname()
  
  if(qu%in%scores){
    head <- paste(l_score(qu), ": ", short_desc,
                  "\r\nMean (SD) (",
                  scale, " scale)", sep="")
    which_test <- "ANOVA"
  } else if (qu %in% yn[1]){
    head <- paste(l(qu), ": ", short_desc,
                  "\r\n % Answering \"Yes\" (SD of prop.) (", scale, " scale)",
                  sep="")
    which_test <- "Kruskal-Wallis"
  } else if (qu %in% yn[4]){
    head <- paste(l(qu), ": ", short_desc,
                  "\r\n % Answering \"No\" (SD of prop.) (", scale, " scale)",
                  sep="")
    which_test <- "Kruskal-Wallis"
  } else {
    head <- paste(l(qu), ": ", short_desc,
                  "\r\nMean (SD) (", scale, " scale)",
                  sep="")
    which_test <- "Kruskal-Wallis"
  }
  
  if(which_test=="Kruskal-Wallis"){
    test_desc <- paste("Kruskal-Wallis Tests for equality of responses to ",
                       l(qu), " between ARAS-PD groups within ",
                       "the row variable group.",
                       sep="")
  } else {
    test_desc <- paste("F-Tests for equality of ",
                       l_score(qu),
                       " between ARAS-PD groups within ",
                       "the row variable group.",
                       sep="")
  }
  
  chi_sq_test_desc <- 
    paste("Three-sample proportion test for equality of ",
          "proportion answering \"Don't Know/Nothing\".",
          sep="")
  
  Z_test_desc <- 
    paste("Wald Z-Test for equality of coefficients in the ", 
          "Student and Faculty models of the ",
          "form Student-Faculty=0.",
          sep="")
  
  ftab <- tab %>%
    dplyr::select(-variable, -Question, -is_or) %>%
    flextable() %>%
    fontsize(size=9) %>%
    #merge_v() %>%
    merge_at(i=5, j=2:4) %>%
    merge_at(i=8, j=2:4) %>%
    merge_at(i=10, j=2:4) %>%
    merge_at(i=12, j=2:4) %>%
    merge_at(i=5, j=6:8) %>%
    merge_at(i=8, j=6:8) %>%
    merge_at(i=10, j=6:8) %>%
    merge_at(i=12, j=6:8) %>%
    #merge_at(i=1:2, j=9, part = "body") %>%
    merge_at(i=4:5, j=10, part = "body") %>%
    merge_at(i=7:8, j=10, part = "body") %>%
    merge_at(i=9:10, j=10, part = "body") %>%
    merge_at(i=11:12, j=10, part = "body") %>%
    delete_part() %>%
    add_header_row(values=c("", rep(c("Low",
                                      "Moderate",
                                      "High",
                                      which_test),
                                    times=2), "Wald")) %>%
    add_header_row(values=c("", rep("Student",4),
                            rep("Faculty",4),
                            "Student vs. Faculty")) %>%
    add_header_row(values=c("Respondent\r\n Characteristics",
                            rep("Mean (SD)", 8),
                            "Student vs. Faculty")) %>%
    merge_h(part="header") %>%
    merge_v(part="header") %>%
    compose(i=1, j=2:9, part = "header",
            value= as_paragraph(head)) %>%
    merge_at(i=1:3, j=1, part="header")
  
  for(l in c(1, 5, 9)){
    for(k in c(1, 4, 7, 9, 11)){
    ftab <- ftab %>%
      merge_at(i=c(k, k+1), j=l, part = "body")
    }
  }
  
  ftab <- ftab %>%
    fix_border_issues() %>%
    theme_box()
  
  if(qu %in% dont_know){
    vals <- c("Don't Know (%)", rep(c("Low", "Moderate", "High",
                                      "Chi-Square"), time=2), "")
    
    ftab <- ftab %>%
      add_footer_row(., values=dk_tab,
                     colwidths=rep(1, 10)) %>%
      add_footer_row(values=vals,
                     colwidths = rep(1, 10)) %>%
      compose(., i=1, j=c(5, 9),
              value=as_paragraph("Prop. Test", " "),
              part = "footer") %>%
      merge_v(part="footer") %>%
      bold(i=1, part="footer") %>%
      theme_box() %>%
      align(align="center", part="footer") %>%
      border(i=1,
             border.top = fp_border(width=1.4),
             part="footer")
  }
  
  ftab <- ftab %>%
    bg("grey92", i=1:12, j=c(5, 9, 10), part="body") %>%
    bg("grey92", i=c(2, 5, 8, 10, 12),
       j=c(2:4, 6:8), part="body") %>%
    align(align="center", part="all") %>%
    align(i=c(5, 8, 10, 12), j=c(2:4, 6:8),
          align="left", part="body") %>%
    add_footer_lines(value=c("ns, not significant; *p<(0.05/k); **p<(0.01/k); ***p<(0.001/k) where k is the number of tests in a \"family\". For Wald tests, k=4; Chi-square tests, k=1; odds ratios and Kruskal-Wallis tests, k=6.")) %>%
    footnote(i=1, j=2, value=as_paragraph(long_desc),
             ref_symbols = c("(1)"),
             part="header",
             inline=T) %>%
    footnote(i=3, j=c(5, 9), value=as_paragraph(test_desc),
             ref_symbols = c("(2)"),
             part="header",
             inline=T) %>%
    footnote(i=3, j=10, value=as_paragraph(Z_test_desc),
             ref_symbols = c("(3)"),
             part="header",
             inline=T) %>%
    {if(borders==T){border(.,
                           j=c(1, 5, 9),
                           border.right = fp_border(width=1.4),
                           part="body") %>%
        border(., i=1:2, 
               j=c(1, 5, 9),
               border.right = fp_border(width=1.4),
               part="footer") %>%
        border(., i=1,
               border.top = fp_border(width=1.4),
               part="body")}else{.}} 
  
  if(qu %in% dont_know){
    ftab <- ftab %>%
      footnote(i=1, j=c(5, 9),
               value=as_paragraph(chi_sq_test_desc),
               ref_symbols = c("(4)"),
               part="footer",
               inline=T)
  }
  
  ftab <- ftab %>%
    width(j=1, 1.3) %>%
    width(j=10, 1) %>%
    width(j=c(2:9), 0.93)
  
  if(qu == "q21a"){
    ftab <- compose(ftab, i=8, j=6:8,
                    value=as_paragraph("OR vs. Bio (SE): NA"))
    ftab <- compose(ftab, i=7:8, j=9:10,
                    value=as_paragraph("NA"))
  }
  
  if(qu == "q13"){
    ftab <- compose(ftab, i=7:8, j=9,
                    value=as_paragraph("8.01 ns"))
  }
  
  return(ftab)
  
}
