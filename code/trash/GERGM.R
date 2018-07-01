install.packages("GERGM")
install.packages("devtools")
devtools::install_github("matthewjdenny/GERGM")
library(GERGM)
library(stargazer)
set.seed(12345)
data("lending_2005")
data("covariate_data_2005")
data("net_exports_2005")
plot_network(lending_2005) 
head(covariate_data_2005)
class(lending_2005)
class(covariate_data_2005)

formula <- lending_2005 ~ edges + mutual(alpha = 0.8) + sender("log_GDP") + 
  receiver("log_GDP") + nodemix("G8", base = "No") + netcov(net_exports_2005) 

test <- gergm(formula,
              covariate_data = covariate_data_2005,
              number_of_networks_to_simulate = 40000,
              thin = 1/100,
              proposal_variance = 0.05,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)
#stargazer(test, type = "tezt")

# Generate Estimate Plot
Estimate_Plot(test)
# Generate GOF Plot
GOF(test)
# Generate Trace Plot
Trace_Plot(test)

Estimate_Plot(test,
              coefficients_to_plot = "both",
              coefficient_names = c("Mutual Dyads",
                                    "log(GDP) Sender",
                                    "log(GDP) Receiver",
                                    "Non-G8 Sender, G8 Receiver",
                                    "G8 Sender, Non-G8 Receiver",
                                    "G8 Sender, G8 Receiver",
                                    "intercept",
                                    "Normalized Net Exports",
                                    "Dispersion Parameter"),
              leave_out_coefficients = "intercept")
# Generate Hysteresis plots for all structural parameter estimates
hysteresis_results <- hysteresis(test,
                                 networks_to_simulate = 1000,
                                 burnin = 300,
                                 range = 8,
                                 steps = 20,
                                 simulation_method = "Metropolis",
                                 proposal_variance = 0.05)



calculate_network_statistics(var.myl.fevd[[1]], weights = NULL,
                             downweight_statistics_together = TRUE)


test <- gergm(formula,
              covariate_data = covariate_data_2005,
              number_of_networks_to_simulate = 40000,
              thin = 1/100,
              proposal_variance = 0.05,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)


gergm(formula, 
      covariate_data = covariate_data_2005, #VIP
      normalization_type = c("log", "division"),# make sure all weights lie in 0 to 1 
      network_is_directed = TRUE, # default is true
      use_MPLE_only = c(FALSE, TRUE), # Logical specifying whether or not only the maximum pseudo likelihood esti- mates should be obtained. In this case, no simulations will be performed. De- fault is FALSE.
      transformation_type = c("Cauchy", "LogCauchy", "Gaussian", "LogNormal"),# Logical specifying whether or not the observed network is directed. Default is TRUE.
      estimation_method = c("Metropolis", "Gibbs"),
      maximum_number_of_lambda_updates = 10,
      maximum_number_of_theta_updates = 10,
      number_of_networks_to_simulate = 500, 
      thin = 1, 
      proposal_variance = 0.1,
      downweight_statistics_together = TRUE, 
      MCMC_burnin = 100, seed = 123,
      convergence_tolerance = 0.5, 
      MPLE_gain_factor = 0,
      acceptable_fit_p_value_threshold = 0.05, 
      force_x_theta_updates = 1,
      force_x_lambda_updates = 1, 
      output_directory = NULL, 
      output_name = NULL,
      generate_plots = TRUE, 
      verbose = TRUE,
      hyperparameter_optimization = FALSE, 
      stop_for_degeneracy = FALSE,
      target_accept_rate = 0.25, 
      theta_grid_optimization_list = NULL,
      beta_correlation_model = FALSE, 
      weighted_MPLE = FALSE,
      fine_grained_pv_optimization = FALSE, 
      parallel = FALSE,
      parallel_statistic_calculation = FALSE, 
      cores = 1,
      use_stochastic_MH = FALSE, 
      stochastic_MH_proportion = 0.25,
      estimate_model = TRUE, 
      slackr_integration_list = NULL)


formula <- lending_2005 ~ edges + mutual(alpha = 0.8) + sender("log_GDP") + 
  receiver("log_GDP") + nodemix("G8", base = "No") + netcov(net_exports_2005) 

test <- gergm(test.y ~ edges  + mutual(alpha = 0.8) + netcov(test.x)+sender("D"),
              covariate_data = temp,
              number_of_networks_to_simulate = 40000,
              thin = 1/100,
              proposal_variance = 0.05,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.5)

test.y <- var.myl.fevd[[1]][1:10,]
colnames(test.y) <- letters[1:10]
rownames(test.y) <- letters[1:10]
test.y %>% as.data.frame

temp <- temp[,3:10]
rownames(temp) <- letters[1:10]
colnames(temp) <- LETTERS[1:10]

test.x <-  var.myl.fevd[[2]][1:10,]#temp.ON[1:10,1:10]
rownames(test.x) <- letters[1:10]
colnames(test.x) <- letters[1:10]
test.x %>% as.data.frame


test@network
test@bounded.network
test@stats
test@theta.coef
test@lambda.coef
test@weights
test@num_nodes
test@MCMC_output
test@observed_network
test@observed_bounded_network
test@data_transformation
test@stats_to_use
test@previous_theta.coef
test@previous_lambda.coef
test@reduced_weights
test@theta.par
test@lambda.par
test@theta_estimation_converged
test@simulated_vs_observed_p_values
test@acceptable_fit  
test@lambda_estimation_converged
test@observed_simulated_t_test
test@console_output
test@print_output
test@is_correlation_network
test@beta_correlation_model
test@directed_network
test@simulation_only
test@thresholds
test@BZ
test@BZstdev
test@transformation_type
test@downweight_statistics_together
test@hyperparameter_optimization
test@target_accept_rate
test@proposal_variance
test@estimation_method
test@number_of_simulations
test@thin
test@burnin
test@MPLE_gain_factor
test@simulated_statistics_for_GOF
test@hessians
test@standard_errors
test@theta_grid_optimization_list
test@using_grid_optimization
test@mu
test@phi
test@weighted_MPLE
test@fine_grained_pv_optimization
test@parallel
test@parallel_statistic_calculation
test@cores
test@use_stochastic_MH
test@stochastic_MH_proportion
test@endogenous_statistic_node_sets
test@non_base_statistic_indicator
test@legacy_statistics
test@legacy_alphas
test@legacy_thetas
test@legacy_thresholds
test@use_legacy_MH_sampler
test@theta_names
test@possible_endogenous_statistic_indices
test@statistic_auxiliary_data
test@full_theta_names
test@covariate_terms_only
test@simulated_bounded_networks_for_GOF
test@additional_stats
test@slackr_integration_list
test@using_slackr_integration
test@start_time