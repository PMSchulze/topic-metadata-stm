# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# install and load required packages
os <- Sys.info()[["sysname"]] # get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # set corresponding installation type
packages_required <- c(
  "devtools", "ggplot2", "grid", "gridExtra", "scales", "stm", "tidyverse"
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
if (length(not_installed) > 0) {
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = itype
  )
}
lapply(packages_required, library, character.only = TRUE)

# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE) # uncomment if stmprevalence doesn't install
devtools::install_github("PMSchulze/stmprevalence") # install from github
library(stmprevalence)

# set working directory (to folder where this code file is saved)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
data <- readRDS("../data/preprocessed_monthly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Hyperparameter Search -------------------------------
# ----------------------------------------------------------------------------------------------

# specify model for use of searchK function
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~")) 

# # search hyperparameter space for optimal K using searchK function
# hyperparameter_search <- stm::searchK(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = c(5,10,15,20,25,30,35,40),
#   prevalence = prevalence,
#   heldout.seed = 123,
#   max.em.its = 200,
#   init.type = "Spectral"
# )
# saveRDS(hyperparameter_search, "../data/searchK_data.rds")

# load searchK results
searchK_data <- readRDS("../data/searchK_data.rds")

# plot four metrics used for hyperparameter search 
plot_heldout <- ggplot(data = searchK_data$results, aes(x = K, y = heldout)) +
  geom_line() +
  geom_point() +
  labs(y = "held-out likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.y = element_text(size = 18, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), limits = c(-8.66, -8.53))

plot_semcoh <- ggplot(data = searchK_data$results, aes(x = K, y = semcoh)) + 
  geom_line() +
  geom_point() +
  labs(y = "semantic coherence") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.y = element_text(size = 18, face = "bold"))

plot_exclus <- ggplot(data = searchK_data$results, aes(x = K, y = exclus)) +
  geom_line() +
  geom_point() +
  labs(y = "exclusivity") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.y = element_text(size = 18, face = "bold"))

plot_residual <- ggplot(data = searchK_data$results, aes(x = K, y = residual)) + 
  geom_line() +
  geom_point() +
  labs(y = "residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
        axis.title.y = element_text(size = 18, face = "bold"))

# create plot and save as pdf
pdf(file = "../plots/searchK.pdf", width = 9, height = 7.5)

gridExtra::grid.arrange(plot_heldout, plot_semcoh, plot_exclus, plot_residual, ncol=2)

dev.off()

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Model Fitting ---------------------------------------
# ----------------------------------------------------------------------------------------------

# # choose covariates and number of topics
# covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
#   s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
# outcome <- ""
# prevalence <- as.formula(paste(outcome, covar, sep = "~"))
K <- 15

# # fit model
# mod_prev <- stm::stm(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = K,
#   prevalence = prevalence,
#   gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 200,
#   init.type = "Spectral")
# saveRDS(mod_prev, "../data/mod_prev_monthly.rds")

# load fitted model
mod_prev <- readRDS("../data/mod_prev_monthly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Word Cloud ------------------------------------------
# ----------------------------------------------------------------------------------------------

# word cloud for selected topic
pdf(file = "../plots/wordcloud_t6.pdf", width = 3.6, height = 3)

stm::cloud(mod_prev, topic = 6, scale = c(2.0, 0.25))

dev.off()

# ----------------------------------------------------------------------------------------------
# ---------------------------------- Plots with estimateEffect ---------------------------------
# ----------------------------------------------------------------------------------------------

# load list of prevalence covariates
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
# load full names of prevalence covariates
varlist_fullnames <- c(
  "time", "party", "federal state", "immigrants (%)", "GDP per capita (EUR)", 
  "unemployement rate (%)", "vote share (%)"
)
formula <- 1:15~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)

# create data
prep <- stm::estimateEffect(
  formula,
  mod_prev,
  metadata = data$meta,
  uncertainty = "Global"
)

# create plot using estimateEffect and save as pdf
times <- c("2017-09", "2018-09", "2019-09")
pdf(file = "../plots/estimateEffect_t6.pdf", width = 9, height = 7.5)

plot(prep, "t", method = "continuous", topics = 6,
     printlegend = F, xlab = "", xaxt="n", ylab = "", linecol = "black", yaxt="n")
axis(1, at=c(0,12,24), labels=times, cex.axis=1.2)
axis(2, cex.axis=1.2)
mtext("date", side=1, line=2.5, cex=1.7, font = 2)
mtext("topic proportion", side=2, line=2.5, cex=1.7, font = 2)

dev.off()

# ----------------------------------------------------------------------------------------------
# --------------------------- Plots with stmprevalence: frequentist ----------------------------
# ----------------------------------------------------------------------------------------------

# # estimate 100 beta regressions and sample from regressions coefficients
# all_betas <- sample_coefs(mod_prev, formula, type = "beta",
#                             data$meta, nsims = 100, seed = 123)
# # save results
# saveRDS(all_betas, "../data/all_betas.rds")

# load previously computed results
all_betas <- readRDS("../data/all_betas.rds")
# predict thetas using beta regression for all variables
preds_beta <- lapply(varlist, 
                     function(v) stmprevalence::predict_props(all_betas, v, formula, data$meta))
names(preds_beta) <- varlist

# create plot and save as pdf
pdf(file = "../plots/freqbeta_t6.pdf", width = 9, height = 7.5)

for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_beta_", v)
  assign(plot_nam, ggplot(preds_beta[[v]]$Topic6, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("topic proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
                 axis.title.x = element_text(size = 18, face = "bold"),
                 axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
                 axis.title.y = element_text(size = 18, face = "bold")))
}
## change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_beta_Date <- plot_beta_t + 
  scale_x_continuous(name = "date", breaks = ticks_date$breaks, labels = ticks_date$labels)
## combine all plots
gridExtra::grid.arrange(
  plot_beta_Date, plot_beta_Struktur_4, plot_beta_Struktur_22, plot_beta_Struktur_42, ncol=2
)

dev.off()

# ----------------------------------------------------------------------------------------------
# ---------------------------- Plots with stmprevalence: Bayesian ------------------------------
# ----------------------------------------------------------------------------------------------

# select topic 6 and covariates
formula <- 6~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
metadata <- data$meta[varlist]
# factorize
metadata[sapply(metadata, is.character)] <- lapply(metadata[sapply(metadata, is.character)], 
                                                   as.factor)

# # obtain MAP estimates of nsims Bayesian beta regressions
# mod_betaregs <- stmprevalence::beta_bayes(mod_prev, formula, metadata, nsims = 100)

# # draw from posterior predictive distribution of previously obtained Beta regressions
# # need to specify quantile (here: 95th, 90th, and 80th percentile)
# preds_beta_bayes_95 <- lapply(varlist, function(x){
#                                stmprevalence::posterior_predict_props(mod_betaregs, x, formula, metadata, 0.025, 0.975)})
# names(preds_beta_bayes_95) <- varlist
# preds_beta_bayes_90 <- lapply(varlist, function(x){
#                                stmprevalence::posterior_predict_props(mod_betaregs, x, formula, metadata, 0.05, 0.95)})
# names(preds_beta_bayes_90) <- varlist
# preds_beta_bayes_85 <- lapply(varlist, function(x){
#   stmprevalence::posterior_predict_props(mod_betaregs, x, formula, metadata, 0.1, 0.9)})
# names(preds_beta_bayes_85) <- varlist

# store sampled values
# saveRDS(preds_beta_bayes_95, "../data/preds_beta_bayes_95.rds")
# saveRDS(preds_beta_bayes_90, "../data/preds_beta_bayes_90.rds")
# saveRDS(preds_beta_bayes_85, "../data/preds_beta_bayes_85.rds")

# load previously calculated results for Bayesian beta regression
preds_beta_bayes_95 <- readRDS("../data/preds_beta_bayes_95.rds")
preds_beta_bayes_90 <- readRDS("../data/preds_beta_bayes_90.rds")
preds_beta_bayes_85 <- readRDS("../data/preds_beta_bayes_85.rds")

# create plot (without credible intervals) and save as pdf
pdf(file = "../plots/stanbeta_t6_noCI.pdf", width = 9, height = 7.5)

for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_betabayes_", v)
  assign(plot_nam, ggplot(preds_beta_bayes_95[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("topic proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
                 axis.title.x = element_text(size = 18, face = "bold"),
                 axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
                 axis.title.y = element_text(size = 18, face = "bold")))
}
## change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_betabayes_Date <- plot_betabayes_t + 
  scale_x_continuous(name = "date", breaks = ticks_date$breaks, labels = ticks_date$labels)
## combine all plots
gridExtra::grid.arrange(plot_betabayes_Date, plot_betabayes_Struktur_4, 
                        plot_betabayes_Struktur_22, plot_betabayes_Struktur_42, ncol=2)

dev.off()

# build single list containing lower and upper bounds of 95th, 90th, and 85th percentile 
preds_beta_bayes_95$t$Topic6$ci_lower_90 <- preds_beta_bayes_90$t$Topic6$ci_lower
preds_beta_bayes_95$t$Topic6$ci_upper_90 <- preds_beta_bayes_90$t$Topic6$ci_upper
preds_beta_bayes_95$Struktur_4$Topic6$ci_lower_90 <- preds_beta_bayes_90$Struktur_4$Topic6$ci_lower
preds_beta_bayes_95$Struktur_4$Topic6$ci_upper_90 <- preds_beta_bayes_90$Struktur_4$Topic6$ci_upper
preds_beta_bayes_95$Struktur_22$Topic6$ci_lower_90 <- preds_beta_bayes_90$Struktur_22$Topic6$ci_lower
preds_beta_bayes_95$Struktur_22$Topic6$ci_upper_90 <- preds_beta_bayes_90$Struktur_22$Topic6$ci_upper
preds_beta_bayes_95$Struktur_42$Topic6$ci_lower_90 <- preds_beta_bayes_90$Struktur_42$Topic6$ci_lower
preds_beta_bayes_95$Struktur_42$Topic6$ci_upper_90 <- preds_beta_bayes_90$Struktur_42$Topic6$ci_upper
preds_beta_bayes_95$Struktur_54$Topic6$ci_lower_90 <- preds_beta_bayes_90$Struktur_54$Topic6$ci_lower
preds_beta_bayes_95$Struktur_54$Topic6$ci_upper_90 <- preds_beta_bayes_90$Struktur_54$Topic6$ci_upper
preds_beta_bayes_95$t$Topic6$ci_lower_85 <- preds_beta_bayes_85$t$Topic6$ci_lower
preds_beta_bayes_95$t$Topic6$ci_upper_85 <- preds_beta_bayes_85$t$Topic6$ci_upper
preds_beta_bayes_95$Struktur_4$Topic6$ci_lower_85 <- preds_beta_bayes_85$Struktur_4$Topic6$ci_lower
preds_beta_bayes_95$Struktur_4$Topic6$ci_upper_85 <- preds_beta_bayes_85$Struktur_4$Topic6$ci_upper
preds_beta_bayes_95$Struktur_22$Topic6$ci_lower_85 <- preds_beta_bayes_85$Struktur_22$Topic6$ci_lower
preds_beta_bayes_95$Struktur_22$Topic6$ci_upper_85 <- preds_beta_bayes_85$Struktur_22$Topic6$ci_upper
preds_beta_bayes_95$Struktur_42$Topic6$ci_lower_85 <- preds_beta_bayes_85$Struktur_42$Topic6$ci_lower
preds_beta_bayes_95$Struktur_42$Topic6$ci_upper_85 <- preds_beta_bayes_85$Struktur_42$Topic6$ci_upper
preds_beta_bayes_95$Struktur_54$Topic6$ci_lower_85 <- preds_beta_bayes_85$Struktur_54$Topic6$ci_lower
preds_beta_bayes_95$Struktur_54$Topic6$ci_upper_85 <- preds_beta_bayes_85$Struktur_54$Topic6$ci_upper

# create plot (with 95th, 90th, and 85th percentile) and save as pdf
pdf(file = "../plots/stanbeta_t6_959085.pdf", width = 9, height = 7.5)

for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_betabayes_", v)
  plot_smoothed_ci <- ggplot(preds_beta_bayes_95[[v]]$Topic6) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower_90), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper_90), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower_85), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper_85), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y,
                               ci_lower_90 = smoothed_ci$data[[3]]$y,
                               ci_upper_90 = smoothed_ci$data[[4]]$y,
                               ci_lower_85 = smoothed_ci$data[[5]]$y,
                               ci_upper_85 = smoothed_ci$data[[6]]$y)  
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey80") +
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower_90, ymax = ci_upper_90), 
                       fill = "grey60") +
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower_85, ymax = ci_upper_85), 
                       fill = "grey40") +
           geom_smooth(data = preds_beta_bayes_95[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion),
                       color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("topic proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text.x = element_text(vjust = 1, size = 14, hjust = 1),
                 axis.title.x = element_text(size = 18, face = "bold"),
                 axis.text.y = element_text(vjust = 1, size = 14, hjust = 1),
                 axis.title.y = element_text(size = 18, face = "bold")))
}

## change axis labeling for time effects
ticks_date <- data.frame(breaks = c(1,13,25), labels = 0)
for (i in ticks_date$breaks) ticks_date[ticks_date["breaks"]==i, "labels"] <- 
  (data$meta$Datum[which(data$meta$t == i)] %>% unique)
plot_betabayes_Date <- plot_betabayes_t + 
  scale_x_continuous(name = "date", breaks = ticks_date$breaks, labels = ticks_date$labels)
## combine all plots
gridExtra::grid.arrange(plot_betabayes_Date, plot_betabayes_Struktur_4, 
                        plot_betabayes_Struktur_22, plot_betabayes_Struktur_42, ncol=2)

dev.off()
