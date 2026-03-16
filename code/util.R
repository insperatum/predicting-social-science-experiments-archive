# This file contains the core analysis functions, and some utilities for saving/loading analysis results
library(tidyverse)
library(metafor)
# library(cocor)

# Function to calculate correlation between experimental estimate and prediction
# This function returns the "raw" correlation and CI (not incorporating errors) as well as the adjusted correlation and CI (incorporating errors)
calc_correlation = function(estimate.rct, V.rct, estimate.pred, return_ci=F, return_profile=F) {
  # estimate.rct: The human experimental estimates
  # V.rct: Covariance matrix for estimate.rct
  # estimate.pred: The model predictions

  # Move data into long format
  data = tibble(est = estimate.rct, pred = estimate.pred) %>%
    mutate(row_id = row_number())
  data = bind_rows( # TODO: cleaner to pivot_longer and arrange by name
    data %>% mutate(name="est", value=est),
    data %>% mutate(name="pred", value=pred)
  )
  
  # Joint covariance matrix for human experimental estimates and model predictions
  V = bdiag(V.rct, diag(0, nrow=length(estimate.rct))) * 1
  V = (V + t(V))/2
  
  # Fit the model
  # (assume estimate.rct and estimate.pred follow a bivariate gaussian)
  options(warn=-1) # Mute metafor warning about non-positive sampling variances for predictions. 
  fit = tryCatch({
    rma.mv(
      data = data, value ~ 0 + name, random=list(~name|row_id), V = V, struct="UN",
      cvvc = TRUE, control = list(hesspack = "numDeriv")
    )
  }, error=function(e) {
    rma.mv(
      data = data, value ~ 0 + name, random=list(~name|row_id), V = V, struct="UN",
      cvvc = TRUE, control=list(hesspack="numDeriv", iter.max=1000, rel.tol=1e-8)
    )})
  options(warn=0)
  
  # If required, calculate the confidence interval and the profile of the log-likelihood
  if(return_ci) {
    ci = confint(fit, rho=1, verbose=T) # Takes 5 minutes...
    out = tibble(estimate=ci$random["rho",'estimate'], ci.lb=ci$random["rho",'ci.lb'], ci.ub=ci$random["rho",'ci.ub'])
    
    if(return_profile) {
      prof = profile(fit, rho=1, parallel="multicore", ncpus=8, steps=201, xlim=c(-1, 1))
      out$profile = list(tibble(rho=prof$rho, ll=prof$ll))
    }
  } else{
    out = tibble(estimate=fit$rho, ci.lb=NA, ci.ub=NA)
  }
  
  out$fit = list(fit)
  out$se = if(any(is.na(fit$vvc))) NA else sqrt(fit$vvc['rho', 'rho'])
  
  # Raw correlation and CI (note that this CI does not incorporate covariance)
  ct = cor.test(estimate.rct, estimate.pred)
  out$cor_raw = ct$estimate[['cor']]
  out$cor_raw_ci.lb = ct$conf.int[[1]]
  out$cor_raw_ci.ub = ct$conf.int[[2]]
  
  return(out)
}



# Function to run a metaregression predicting experimental results from predictions
calc_metaregression = function(estimate.rct, V.rct, estimate.pred) {
  data = tibble(est = estimate.rct, pred = estimate.pred) %>%
    mutate(row_id = row_number())
  
  fit0 = rma.mv(data = data, est ~ 1, random=list(~1|row_id), V = V.rct, sparse=T)
  fit = rma.mv(data = data, est ~ pred, random=list(~1|row_id), V = V.rct, sparse=T)
  
  var.rct = fit0$sigma2
  resid_var.rct = fit$sigma2
  sampling_var.rct = mean(diag(V.rct))
  
  # The slope of the metaregression
  b = broom::tidy(fit) %>% filter(term=="pred") %>% pull(estimate)
  b_conf.low = broom::tidy(fit, conf.int=T) %>% filter(term=="pred") %>% pull(conf.low)
  b_conf.high = broom::tidy(fit, conf.int=T) %>% filter(term=="pred") %>% pull(conf.high)
  
  out = tibble(b, var.rct, b_conf.low, b_conf.high, resid_var.rct, sampling_var.rct)
  return(out)
}


calc_correlation_comparison = function(estimate, V, pred1, pred2) {
  
  data = tibble(est = estimate, pred1, pred2) %>% mutate(row_id = row_number())
  
  data = bind_rows(
    data %>% mutate(name="est", value=est),
    data %>% mutate(name="pred1", value=pred1),
    data %>% mutate(name="pred2", value=pred2),
  )
  
  form = "value ~ 0 + name"
  
  V.pred = diag(0, nrow=length(estimate))
  
  V_all = bdiag(V, V.pred, V.pred) * 1
  V_all = (V_all + t(V_all))/2
  
  options(warn=-1) # Mute metafor warning about non-positive sampling variances for predictions
  fit = rma.mv(
    data = data, as.formula(form), random=list(~name|row_id), V = V_all, struct="UN",
    cvvc = TRUE, control=list(hesspack="pracma", iter.max=1000, rel.tol=1e-8)
  )
  if(any(is.na(fit$vvc))) {
    fit = rma.mv(
      data = data, as.formula(form), random=list(~name|row_id), V = V_all, struct="UN",
      cvvc = TRUE, control=list(hesspack="numDeriv", iter.max=1000, rel.tol=1e-8)
    )
  }
  options(warn=0)
  
  out = tibble(corr_diff_estimate = fit$rho[[1]] - fit$rho[[2]])
  if(! any(is.na(fit$vvc))) {
    out = out %>% mutate(
      corr_diff_se = sqrt(fit$vvc['rho.2.1', 'rho.2.1'] + fit$vvc['rho.3.1', 'rho.3.1'] - 2 * fit$vvc['rho.2.1', 'rho.3.1']),
      corr_diff_lwr = corr_diff_estimate - qnorm(0.975) * corr_diff_se,
      corr_diff_upr = corr_diff_estimate + qnorm(0.975) * corr_diff_se,
      corr_diff_p = 2*(1-pnorm(abs(corr_diff_estimate) / corr_diff_se)),
    )
  }
    
  out
}
  

# This function
# - Loops through a list of specifications (e.g. subsetting on particular studies or participants)
# - For each specification, estimates the correlation and RMSE for every model in a dataframe
# It is used for both Figure 2B (to calculate overall accuracy by model) and Figure 2C (to calculate GPT-4 accuracy for different subsets)
run_sims = function(d, filters) with_progress({
  
  specs = crossing(tibble(run_idx = 1:n_runs), filters)
  create_subsamples(d, specs)
  
  N_nonredundant_contrasts = d %>% group_by(study, outcome.name) %>% slice_head(n=1) %>% ungroup() %>% with(sum(map_dbl(df_reg, nrow)))
  p = progressor(steps = specs %>% pmap_dbl(function(spec_filter, ...) {n_distinct(spec_filter(d)$model)}) %>% sum)
  
  print("Running sims...")
  future_pmap_dfr(specs, .f = function(run_idx, spec, spec_filter) {
    samp = load_subsample(spec, run_idx)
    
    spec_filter(d) %>%
      right_join(samp) %>%
      group_by(model) %>%
      group_modify(function(d,k) {
        out = tryCatch({
          filename = str_glue("output/processed_data/analysis1_sims/{str_replace_all(spec, '[^a-zA-Z0-9 ]', '_')} - {str_replace_all(k$model, '[^a-zA-Z0-9 ]', '_')} {run_idx}.rds")
          run_cached(filename, \() {
            
            # TODO simpler:
            # d_samp1 %>% unnest(c(df_reg, df_llm), names_sep=".")
            df_est = d %>%
              unnest(df_reg) %>%
              unnest(df_llm, names_sep="__") %>%
              filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction)
            
            V = bdiag(d$V)
            
            out = mutate(
              calc_correlation(df_est$estimate, V, df_est$prediction, return_ci = T),
              calc_metaregression(df_est$estimate, V, df_est$prediction),
              rmse = sqrt(mean((df_est$prediction - df_est$estimate)^2)),
              rmse_adj = sqrt(mean((df_est$prediction - df_est$estimate)^2 - diag(V)))
            )
            
            # Calculate SEs for MSE.
            # TODO: are we even using this?
            mse_se = function(y, p, V) {
              n = length(y); stopifnot(length(p)==n, nrow(V)==n, ncol(V)==n)
              V  = as.matrix((V + t(V))/2)  # symmetrize
              r  = p - y
              trV2    = sum(V * V)          # tr(V^2)
              term1   = as.numeric(t(r) %*% V %*% r)
              var_mse = (4*term1 - 2*trV2) / (n^2)
              se_mse  = sqrt(var_mse)
              se_mse
            }
            out %>% mutate(mse_adj_se = mse_se(df_est$estimate, df_est$prediction, V))
          })
        }, error = function(err) { return(tibble(error=list(err))) })
        
        p()
        out
      }) %>%
      ungroup() %>%
      mutate(run_idx, spec, N_nonredundant_contrasts)
  })
  
})


# This function is simular to run_sims, but is used to calculate statistics for comparisons between prediction methods (e.g. GPT-4 vs human)
run_comparison_sims = function(d, filters) with_progress({
  specs = crossing(tibble(run_idx = 1:n_runs), filters)
  # n = specs %>% pmap_dbl(function(spec_filter, ...) {n_distinct(spec_filter(d)$model)}) %>% sum
  p = progressor(steps = nrow(specs))
  
  future_pmap_dfr(specs, .f = function(run_idx, spec, spec_filter) run_cached(str_glue("output/processed_data/analysis1_sims/comparison {spec} {run_idx}.rds"), \() {
    samp = load_subsample(spec, run_idx)
    
    d_samp = spec_filter(d) %>% right_join(samp)
    d_human = d_samp %>% filter(model=="human")
    d_gpt = d_samp %>% filter(model=="gpt-4")
    d_gpt3.5 = d_samp %>% filter(model=="gpt-3.5-turbo")
    d_combined = d_samp %>% filter(model=="combined")
    
    df_est_human = d_human %>%
      unnest(df_reg) %>%
      unnest(df_llm, names_sep="__") %>%
      filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction)
    df_est_gpt = d_gpt %>%
      unnest(df_reg) %>%
      unnest(df_llm, names_sep="__") %>%
      filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction)
    df_est_gpt3.5 = d_gpt3.5 %>%
      unnest(df_reg) %>%
      unnest(df_llm, names_sep="__") %>%
      filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction)
    df_est_combined = d_combined %>%
      unnest(df_reg) %>%
      unnest(df_llm, names_sep="__") %>%
      filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction)
    
    V = bdiag(d_human$V)
    
    f = function(y, p1, p2) { #y is RCT estimate, p1 and p2 are predictions
      n = length(y); stopifnot(length(p1)==n, length(p2)==n, nrow(V)==n, ncol(V)==n)
      V  = as.matrix((V + t(V))/2) # covariance on ATEs
      
      # Betas
      m = rma.mv(y ~ p1 + p2, V=V) %>% broom::tidy(conf.int=T)
      beta1_estimate = m[m$term=="p1", "estimate"][[1]]
      beta1_lwr = m[m$term=="p1", "conf.low"][[1]]
      beta1_upr = m[m$term=="p1", "conf.high"][[1]]
      beta1_p = m[m$term=="p1", "p.value"][[1]]
      beta2_estimate = m[m$term=="p2", "estimate"][[1]]
      beta2_lwr = m[m$term=="p2", "conf.low"][[1]]
      beta2_upr = m[m$term=="p2", "conf.high"][[1]]
      beta2_p = m[m$term=="p2", "p.value"][[1]]
      
      # Difference in MSE
      d = p1 - p2
      U = (p1 - y)^2 - (p2 - y)^2
      Dmat   = diag(as.numeric(d))
      Sigma  = 4 * Dmat %*% V %*% Dmat   # n x n
      unit = factor(seq_len(n))  # each observation is its own 'study'
      fit_re = rma.mv(
        yi = U,
        V = Sigma,
        random = ~ 1 | unit,
        method = "REML"
      )
      
      mse_stats = fit_re %>% broom::tidy(conf.int=T)
      mse_diff_estimate = mse_stats$estimate[[1]]
      mse_diff_lwr = mse_stats$conf.low[[1]]
      mse_diff_upr = mse_stats$conf.high[[1]]
      mse_diff_p = mse_stats$p.value[[1]]
      
      
      # Difference in correlation
      out = tibble(
        beta1_estimate, beta1_lwr, beta1_upr, beta1_p,
        beta2_estimate, beta2_lwr, beta2_upr, beta2_p,
        mse_diff_estimate, mse_diff_lwr, mse_diff_upr, mse_diff_p
      )
      
      tryCatch({
        comp = calc_correlation_comparison(y, V, p1, p2)
        p()
        return(out %>% mutate(comp, error=NA))
      }, error=function(e) {
        p()
        return(out %>% mutate(error = list(e)))
      })
    }
    
    bind_rows(
      f(df_est_human$estimate, df_est_gpt$prediction, df_est_human$prediction) %>% mutate(comparison="gpt-vs-human"),
      f(df_est_human$estimate, df_est_gpt3.5$prediction, df_est_human$prediction) %>% mutate(comparison="gpt3.5-vs-human"),
      f(df_est_human$estimate, df_est_combined$prediction, df_est_human$prediction) %>% mutate(comparison="combined-vs-human"),
      f(df_est_human$estimate, df_est_combined$prediction, df_est_gpt$prediction) %>% mutate(comparison="combined-vs-gpt"),
    ) %>% mutate(run_idx, spec)
  }))
})



# Helper function to save variables for export to latex
save_vars = function(df_vars, filename) {
  df_vars %>%
    with(str_glue("\\expandafter\\def\\csname {name}\\endcsname{{{value}}}")) %>%
    paste0(collapse="\n") %>%
    write_lines(str_glue("../results/variables/{filename}"))
}



# Helper function to save and load analysis results
run_cached = function(filename, f) {
  if(! use_analysis_cache) return(f())
  
  if(file.exists(filename)) {
    cat("Reading", filename, "\n")
    # Analysis has already been run, just load the results
    if(str_ends(str_to_lower(filename), ".csv")) {
      return(read_csv(filename))
    } else if(str_ends(str_to_lower(filename), ".rds")) {
      return(read_rds(filename))
    } else {
      stop()
    }
  } else {
    # Run the analysis, and then save the results
    output = f()
    
    cat("WRITING TO ", filename, "\n")

    if(str_ends(str_to_lower(filename), ".csv")) {
      write_csv(output, filename)
    } else if(str_ends(str_to_lower(filename), ".rds")) {
      write_rds(output, filename, compress = "gz")
    } else {
      stop()
    }
    cat("DONE!\n")
    
    return(output)
  }
}

# Helper functions for saving/loading the data subsamples used in archive 1 analysis
if (!dir.exists("output/processed_data/analysis1_sims")) dir.create("output/processed_data/analysis1_sims")
if (!dir.exists("output/processed_data/analysis1_subsample")) dir.create("output/processed_data/analysis1_subsample")
create_subsamples = function(d, specs) with_progress({
  print("Creating subsamples...")
  p = progressor(steps=nrow(specs))
  specs %>% pmap(.f = function(run_idx, spec, spec_filter) {
    filename_samp = str_glue("output/processed_data/analysis1_subsample/{str_replace_all(spec, '[^a-zA-Z0-9 ]', '_')} {run_idx}.rds")
    set.seed(run_idx)
    
    samp = run_cached(filename_samp, \() {
      spec_filter(d) %>%
        group_by(study) %>%
        filter(reference_condition == sample(reference_condition, 1)) %>%
        filter(outcome.name == sample(outcome.name, 1)) %>%
        ungroup() %>%
        select(study, reference_condition, outcome.name) %>%
        distinct
    })
    p()
  })
})
load_subsample = function(spec, run_idx) {
  readRDS(str_glue("output/processed_data/analysis1_subsample/{str_replace_all(spec, '[^a-zA-Z0-9 ]', '_')} {run_idx}.rds"))
}