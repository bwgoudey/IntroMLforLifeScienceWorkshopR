
plot_roc_auc <- function(yp_train_df, yp_test_df, y_train, y_test, label, base_size=25) {
  
  preds <- yp_train_df %>%
    bind_cols(y_train) %>%
    mutate(subset="train") %>%
    rbind(
      yp_test_df %>%
        bind_cols(y_test) %>%
        mutate(subset="test")
    ) %>%
    mutate(subset=factor(subset, c("train", "test")))
  
  auc_df <- preds %>%
    group_by(subset) %>%
    yardstick::roc_auc(truth = diabetes, .pred_0)  %>%
    rename(auc=.estimate)
  
  roc_curve_df <- preds %>%
    group_by(subset) %>%
    yardstick::roc_curve(truth = diabetes, .pred_0)
  
  metrics_df = roc_curve_df %>%
    left_join(auc_df, by='subset')
  
  # Plot training and external validation ROC curves
  metrics_df %>%
    ggplot(aes(x=1-specificity, y=sensitivity, color=subset)) +
    geom_line() +
    labs(title = paste(label, "ROC Curve"),
         x = "False Positive Rate (1-Specificity)",
         y = "True Positive Rate (Sensitivity)") +
    theme_light(base_size = base_size) +
    geom_text(
      aes(x = 0.6, y = 0.3, label = paste("AUC =", round(auc,3))),
      size = base_size/2,  inherit.aes = FALSE
    ) +  facet_wrap(~subset, nrow=1) + theme(legend.position = 'none')
}





# Wrap the split in an rset object
df_to_rset <- function(df1, df2, all=FALSE, df3=NULL) {
  
  diabetes_df_all = rbind(
    df1 %>% mutate(study="Study1"), 
    df2 %>% mutate(study="Study2")
  )
  I_study1=which(diabetes_df_all$study=="Study1")
  I_study2=which(diabetes_df_all$study=="Study2")
  
  indicies = list(list(analysis=I_study1, assessment=I_study1), 
                  list(analysis=I_study1, assessment=I_study2))
  
  splits <- lapply(indicies, make_splits, data = diabetes_df_all %>% dplyr::select(-study))
  train_test_split_rset <- manual_rset(splits, c("Train:Dataset1,Test=Dataset1", 
                                                 "Train:Dataset1, Test:Dataset2"))
  train_test_split_rset
}

initial_split_to_rset <- function(inital_split_obj, df, all=FALSE, df2=NULL) {
  # Wrap the split in an rset object
  indicies = list(
    list(analysis=inital_split_obj$in_id, 
         assessment=inital_split_obj$in_id), 
    list(analysis=inital_split_obj$in_id,
         assessment=setdiff(1:nrow(df), inital_split_obj$in_id)))
  
  if(all) {
    diabetes_df_all = rbind(
      df %>% mutate(study="Study1"), 
      df2 %>% mutate(study="Study2")
    )
    I_study1=which(diabetes_df_all$study=="Study1")
    I_study2=which(diabetes_df_all$study=="Study2")
    stopifnot(!is.null(df2))
    indicies = append(list(list(analysis=I_study1, assessment=I_study1), 
                           list(analysis=I_study1, assessment=I_study2)), 
                      indicies)
    
    splits <- lapply(indicies, make_splits, data = diabetes_df_all %>% dplyr::select(-study))
    train_test_split_rset <- manual_rset(splits, c("Train:All,Test:All", 
                                                   "Train:Training,Test=Training", 
                                                   "Train:Training,Test=Test",
                                                   "Train:Dataset1, Test:Dataset2"))
  } else {
    splits <- lapply(indicies, make_splits, data = df)
    train_test_split_rset <- manual_rset(splits, c("Training", "Testing"))
  }
}




initial_split_to_rset <- function(inital_split_obj, df, all=FALSE, df2=NULL) {
  # Wrap the split in an rset object
  indicies = list(
    list(analysis=inital_split_obj$in_id, 
         assessment=inital_split_obj$in_id), 
    list(analysis=inital_split_obj$in_id,
         assessment=setdiff(1:nrow(df), inital_split_obj$in_id)))
  
  if(all) {
    diabetes_df_all = rbind(
      df %>% mutate(study="Study1"), 
      df2 %>% mutate(study="Study2")
    )
    I_study1=which(diabetes_df_all$study=="Study1")
    I_study2=which(diabetes_df_all$study=="Study2")
    stopifnot(!is.null(df2))
    indicies = append(list(list(analysis=I_study1, assessment=I_study1), 
                           list(analysis=I_study1, assessment=I_study2)), 
                      indicies)
                      
    splits <- lapply(indicies, make_splits, data = diabetes_df_all %>% dplyr::select(-study))
    prop=round(100*length(train_test_split$in_id)/nrow(df))
    train_test_split_rset <- manual_rset(splits, 
                                         c("Train:Dataset1,Test:Dataset1",  
                                           "Train:Dataset1, Test:Dataset2",
                                           sprintf("Train:%d%% Dataset1,Test=%d%% Dataset1",prop,prop), 
                                           sprintf("Train:%d%% Dataset1,Test=%d%% Dataset1",prop,100-prop)))
  } else {
    splits <- lapply(indicies, make_splits, data = df)
    train_test_split_rset <- manual_rset(splits, c("Training", "Testing"))
  }
}


append_rand_feat <- function(data_df, add_n_features = 20) {
  rand_feats <- matrix(runif(nrow(data_df) * add_n_features), nrow = nrow(data_df), ncol = add_n_features)
  rand_feats <- scale(rand_feats)
  feat_names <- paste0("random_", seq_len(add_n_features))
  rand_feats_df <- as.data.frame(rand_feats)
  colnames(rand_feats_df) <- feat_names
  return(bind_cols(data_df, rand_feats_df))
}

load_diabetes_data <- function(diabetes_df_raw, nsamples=1000, add_n_features=100,seed=1) {
  set.seed(seed) 
  
  diabetes_df = diabetes_df_raw %>% 
    dplyr::select(-id,-year_of_followup) %>% #, -censor_of_diabetes_at_followup_1_yes_0_no,-X) %>% 
    mutate(diabetes=factor(diabetes)) 
  
  if(add_n_features>0) {
    diabetes_df = append_rand_feat(diabetes_df, add_n_features)
  }
  
  nsamples= min(nsamples, nrow(diabetes_df), na.rm = T)
  diabetes_study1_df=diabetes_df %>% filter(site==5) %>% group_by(diabetes) %>% slice_head(n=ceiling(nsamples/2)) %>% ungroup()
  diabetes_study2_df=diabetes_df %>% filter(site!=5) %>% group_by(diabetes) %>% ungroup()
  
  diabetes=list(study1=diabetes_study1_df %>% dplyr::select(-site), 
                study2=diabetes_study2_df%>% dplyr::select(-site))
  
  return(diabetes)
}

