
# Function to add random noise to a data frame
generate_random_features <- function(sim_df, n_random = 20) {
  random_features <- matrix(rnorm(nrow(sim_df) * n_random), nrow = nrow(sim_df))
  
  random_df <- as.data.frame(scale(random_features))  # Standard scaling
  
  colnames(random_df) <- paste0("noise_", seq_len(n_random) - 1)
  return(random_df)
}

binarize_y <- function (y)  {
  return(factor(1*(y > 100)))
}


# Generate novel data based on the existing dataset
generate_novel_data <- function(df, n_samples = 1000, n_random = 20) {
  
  sim_df <- mvrnorm(n_samples, mu = colMeans(df), Sigma = 3*cov(df)) %>% 
    as.data.frame() %>% 
    mutate(y=binarize_y(y_cont)) 
  
  if (n_random > 0) {
    sim_df2 <- generate_random_features(sim_df, n_random)
  } else {
    sim_df2=sim_df
  }
  
  return(cbind(sim_df, sim_df2))
}


# Load the diabetes data, tidy up, binarize y, add noise
load_diabetes_data <- function(diabetes_path = './sklearn_diabetes_dataset.csv', n_random = 20, n_samples=1000) {
  # Read the dataset and make sure it's similar to sklearn's format
  diabetes_df <- read.csv(file = diabetes_path) %>% dplyr::select(-ind)
  
  # The defaul names are a bit strange so we clean then up
  diabetes_df <- diabetes_df %>% 
    rename(
      tc = s1,
      ldl = s2,
      hdl = s3,
      tch = s4,
      ltg = s5,
      glu = s6,
      y_cont=y
    ) %>% 
    mutate(y= binarize_y(y_cont))
  
  # # Target variable is a quantitative measure of disease progression one year after baseline
  # that we dichotimise (turn into two groups) to simplify some analysis.
  
  novel_data <- generate_novel_data(diabetes_df %>% dplyr::select(-y), 
                                    n_samples = n_samples, 
                                    n_random = n_random) %>% as.data.frame()
  
  
  #If we've specified, add some number of randomly generated features
  if (n_random > 0) {
    diabetes_df <- cbind(diabetes_df, generate_random_features(diabetes_df, n_random))
  }
  
  #Check our simulated data has the same columns as the oriignal data
  stopifnot(all(colnames(diabetes_df) %in% colnames(novel_data)))
  
  list(original=diabetes_df,  
       simulated = novel_data)
}


# Plot an ROC curve using ggplot2
plot_roc <- function(y, yp, label = "") {
  roc_obj <- roc(y, yp)
  df <- data.frame(
    FPR = roc_obj$specificities * -1 + 1,  # Calculating False Positive Rate
    TPR = roc_obj$sensitivities,           # True Positive Rate
    AUC = round(auc(roc_obj), 2)
  )
  
  ggplot(df, aes(x = FPR, y = TPR)) +
    geom_line() +
    geom_area(alpha = 0.2) +
    labs(title = paste(label, "ROC Curve"), x = "False Positive Rate", y = "True Positive Rate") +
    annotate("text", x = 0.6, y = 0.3, label = paste("AUC =", df$AUC[1]), size = 5, color = "blue") +
    theme_minimal()
}