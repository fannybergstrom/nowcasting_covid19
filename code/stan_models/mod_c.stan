// Model description:
// Perform Bayesian hierarchical Nowcast with log-linear model for 
// log(lambda_{t}) = N(beta_0 + beta_1*lead_ind_1_t + beta_2*lead_ind_2_t, sigma), 
// Model observed case counts n_{t,d} ~ NB(lambda[t]*p_{t,d}, phi), 
// with phi over-dispersion
// Delay distribution: Discrete time-hazard model with week-day effects
data {
  // Data
  int T;              // Number of rows in reporting triangle 
  int D;              // Maximum delay and number of columns of reporting triangle'
  int r[T, D];        // Reporting triangle (Excluding zero delay)
  real lead_ind_1[T]; // First lead indicator
  real lead_ind_2[T]; // Second lead indicator
  int k_wd_haz;       // Number covariates discrete-time hazard model
  matrix[T, k_wd_haz] W_wd[D];  // Design matrix for discrete hazard model
  matrix[T, D] Z;     // Matrix indicating non-reporting days
  // Prior parameter
  vector[D] alpha;  // Parameters of Dirichlet prior for baseline delay distribution
}

parameters {
  simplex[D] p_bl_pr; // Delay probabilities
  vector[T] epsilon;   // Error log-linear model (scaled by sigma)
  // Epi-curve model
  real<lower=0, upper=2> sigma; // Variance parameter for random walk
  real beta_0; // Intercept
  real beta_1; // Association coefficient
  real beta_2; // Association coefficient
  // Reporting model
  // Week-day effect
  vector[k_wd_haz] beta_wd_haz;       
  // Hyperprior
  real<lower=0> sd_beta_wd_haz;
  // Data model
  real<lower=0, upper=1> reciprocal_phi; // dispersion parameter: var=mu+reciprocal_phi*mu^2
}

transformed parameters {
  // Hospitalization model
  vector[T] logLambda; // Expected number of cases at time t

  // reporting model
  vector[D-1] gamma;   // Discrete hazard model intercept
  matrix[T, D] h;      // Discrete hazard w.r.t time and delay
  matrix[T, D] p;      // Reporting probability w.r.t. time and delay
  
  // Data model
  real phi;
  // Discrete hazard model
  gamma = logit((p_bl_pr ./ cumulative_sum(p_bl_pr[(D):1])[(D):1])[1:(D-1)]);
  
  for (d in 1:(D-1)){
    h[, d] = inv_logit(gamma[d] + W_wd[d]*beta_wd_haz) .* (rep_vector(1, T) - Z[, d]);
    if (d==1) {
        p[, d] = h[, d];
      } else {      
        p[, d] = h[, d] .* (1 - (p[, 1:(d-1)] * rep_vector(1, d-1)));
      }
  }
  h[, D] = rep_vector(1, T);
  p[, D] = 1 -  (p[, 1:(D-1)] * rep_vector(1, D-1));
  // Log-lambda
  for(t in 1:T) {
    logLambda[t] = beta_0 + beta_1 * lead_ind_1[t] + beta_2 * lead_ind_2[t] + sigma * epsilon[t]; 
  }
  // Overdispersion
  phi = 1 / reciprocal_phi;
}

model {
  // Priors
  sigma ~ normal(0, .5); // scale of the error-term

  // reporting delay
  // Hyper-prior
  p_bl_pr ~ dirichlet(alpha);
  sd_beta_wd_haz ~ normal(0, 0.5);
  // Prior
  beta_wd_haz ~ normal(0, sd_beta_wd_haz);

  // log-Lambda
  beta_0 ~ normal(0,2);
  beta_1 ~ normal(0,0.5);
  beta_2 ~ normal(0,0.5);
  epsilon ~ std_normal();
  // Model for observed counts
  
  for (t in 1:T) {
    for (d in 1:min(T - t + 1, D)) {
      if (p[t, d] > 0){
        r[t, d] ~ neg_binomial_2(exp(logLambda[t]) * p[t, d], phi);
      }
    }
  }
}

generated quantities {
   int n[D, D];
  int N[D];
  for (t in (T-(D-1)):T){
    for (d in 1:D){
      if (t + d <= T){
        n[t-T+D, d] = r[t, d];
      } else {
        if (p[t, d] > 0){
          n[t-T+D, d] = neg_binomial_2_rng(exp(logLambda[t]) * p[t, d], phi);
        } else {
          n[t-T+D, d] = 0;
        }
      }
    }
    N[t-T+D] = sum(n[t-T+D, ]);
  }
}