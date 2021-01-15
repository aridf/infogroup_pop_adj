#' function to get fit statistics
get_errors <- function(y, y_hat, name, round = FALSE) {
  metrics <- as.data.frame(matrix(ncol = 0, nrow = 1))
  
  # Check for missing values
  na_prds <- sum(is.na(y_hat))
  if (na_prds > 0) {
    warning(paste("Some predictions are missing. Dropping", na_prds, "rows."))
    y <- y[-which(is.na(y_hat))]
    y_hat <- y_hat[-which(is.na(y_hat))]
  }
  
  # Add the mean and sd of y and y_hat
  metrics$mean_y <- mean(y)
  metrics$sd_y <- sd(y)
  metrics$mean_yh <- mean(y_hat)
  metrics$sd_yh <- sd(y_hat)
  
  # Correlation
  metrics$r <- cor(y, y_hat)
  
  # R2 
  metrics$r2 <- cor(y, y_hat)^2
  
  # RMSE
  metrics$rmse <- sqrt(mean((y_hat - y)^2))
  
  # MAD
  metrics$mad <- mean(abs(y_hat - y))
  
  # Median absolute deviation
  metrics$med_ad <- median(abs(y_hat - y))
  
  # If round is true, round the whole dataframe
  if (round){
    metrics <- round(metrics, 6)
  }
  
  rownames(metrics) <- name
  return(metrics)
}

#' function to fit a list of formulas on the same dataset
fit_models <- function(
  formula_list, 
  data, 
  file = getwd(), 
  cores = getOption("mc.cores", 1),
  prior = NULL,
  chains = 4,
  iter = 2000,
  warmup = iter/2,
  thin = 1,
  bayes = "some"
) {
  models <- list()
  for(ii in seq_along(formula_list)) {
    
    message(cat("Fitting model", ii))
    
    f <- formula_list[[ii]]
    
    strt <- Sys.time()
    
    
    if (bayes == "all" | (bayes == "some" & any(grepl("\\|", as.character(f))))) {
      model <- brm(
        f, 
        data, 
        file = paste0(file),
        cores = cores,
        prior = prior,
        chains = chains,
        iter = iter,
        warmup = warmup,
        thin = thin
      )
    } else if (bayes == "none") {
      model <- lmer(f, data)  
    } else if (bayes == "some") {
      model <- lm(f, data)
    } else {
      stop("invalid 'bayes' entry")
    }
    models[[ii]] <- model
    message(cat("Model fit complete. Runtime: ", Sys.time() - strt))
  }
  return(models)
}

#' Function to get predicted values and standard errors from a list of models
get_preds <- function(models, newdata, n_samples = NULL, resids = FALSE) {
  
  for(ii in seq_along(models)) {
    
    mod <- models[[ii]]
    
    if(class(mod) == "brmsfit") {
      pr <- as.data.frame(
        fitted(
          mod, 
          newdata,
          cores = parallel::detectCores(),
          nsamples = n_samples,
          probs = c(0.05, 0.95)
        )
      )
      
      newdata <- newdata %>%
        mutate(
          "lprd_{ii}" := pr$Estimate,
          "lu90_{ii}" := pr$Q95,
          "ll90_{ii}" := pr$Q5,
          
          "prd_{ii}" := exp(pr$Estimate),
          "u90_{ii}" := exp(pr$Q95),
          "l90_{ii}" := exp(pr$Q5)  
        )
      
    } else {
      pr <- predict(mod, newdata, se.fit = TRUE)
      
      newdata <- newdata %>%
        mutate(
          "lprd_{ii}" := pr$fit,
          "lu95_{ii}" := pr$fit + pr$se.fit,
          "ll95_{ii}" := pr$fit - pr$se.fit,
          
          "prd_{ii}" := exp(pr$fit),
          "u95_{ii}" := exp(pr$fit) + exp(pr$se.fit),
          "l95_{ii}" := exp(pr$fit) - exp(pr$se.fit),
        )     
      if (resids) {
        newdata <- newdata %>%
          mutate(
            "lres_{ii}" := mod$residuals
          )
      }
    }
  }
  return(newdata)
}

#' function to project years 
proj_by_year <- function(
  newdata, 
  decennial_data, 
  models, 
  geography_label,
  n_samples = NULL
) {
  
  all_predictions <- list()
  
  for(ii in seq_along(years)) {
    
    y <- years[ii]
    
    message("predictions for ", y)
    
    # Drop out the variables from decennial that we want replaced 
    # with new data from the year being projected.
    # Specifically, we are dropping the 2010 counts, the year 2010,
    # and the census counts from 2010
    decennial_minus_ig <- decennial_data %>% 
      select(-ig_count_imptd, -year, -chh)
    
    # Add in the new data from the ACS-IG dataset.
    # This adds in the ig counts, census counts, and the year variable
    # from the year being projected.
    suppressMessages({
      to_predict <- newdata %>% 
        select(one_of(geography_label), year, ig_count_imptd, chh, chh_moe) %>% 
        filter(year == y) %>%
        inner_join(decennial_minus_ig)
      
      name <- paste0("prd_", y)
      
      # Here we do the actual projection
      all_predictions[[name]] <- get_preds(
        models, 
        to_predict, 
        n_samples = n_samples
      )
    })
  }
  
  # collapsing it all into one dataframe here
  all_predictions <- bind_rows(all_predictions)
  
  return(all_predictions)
}

#' Function to get errors for each year
get_errors_by_year <- function(projections, years, varname, log = TRUE) {
  
  all_errors <- list()
  
  for(ii in seq_along(years)) {
    
    y <- years[ii]
    
    to_get_errors <- projections %>% 
      filter(year == y)
    
    name <- paste0("errors_", y)
    
    if(log) {
      groundtruth <- log(to_get_errors$chh)
    } else {
      groundtruth <- to_get_errors$chh
    }
    
    all_errors[[name]] <- get_errors(groundtruth, unlist(to_get_errors[varname]), name)
    
  }
  return(bind_rows(all_errors))
}

# function to create 5 year averages of a given prediction
get_fiveyear <- function(data, varnames, geography_label) {
  
  years <- sort(unique(data$year))
  yr <- years[5]
  end_yr <- years[length(years)]
  
  res <- list()
  
  while(yr < end_yr) {
    
    message("Averaging for ", yr)
    
    # get five year average
    aggs <- data %>%
      filter(year %in% c(yr:(yr - 5))) %>%
      select(one_of(geography_label), one_of(c(varnames))) %>%
      group_by_at(geography_label) %>%
      summarize_at(varnames, mean, na.rm = FALSE) %>%
      mutate(year = yr)
    
    # fix names
    names(aggs)[which(names(aggs) %in% varnames)] <- paste0(
      names(aggs)[which(names(aggs) %in% varnames)], 
      "_5year"
    ) 
    
    # add to list
    name <- paste0("aggs", yr)
    res[[name]] <- aggs 
    
    yr <- yr + 1
  } 
  
  # Put it all together
  suppressMessages({
    res <- res %>%
      bind_rows %>%
      inner_join(data)
  })
}
