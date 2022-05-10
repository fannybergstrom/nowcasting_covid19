// Model description:
// Perform Bayesian hierarchical Nowcast with log-linear model for 
// log(lambda_{t}) = N(beta_0 + beta_1*lead_ind_1_t + beta_2*lead_ind_2_t, sigma), 
// Model observed case counts n_{t,d} ~ NB(lambda[t]*p_{t,d}, phi), 
// with phi over-dispersion
// Delay distribution: Discrete time-hazard model with week-day effects
// Model description:
// Perform Bayesian hierarchical Nowcast with log-linear model for 
// log(lambda_{t}) = N(beta_0 + beta_1*lead_ind_1 + beta_2*lead_ind_2, sigma), 
// model observed case counts n_{t,d} ~ NB(lambda[t]*p_{t,d}, phi), 
// with phi over-dispersion
// delay distribution: Discrete time-hazard model with week-day effects

data {
  // Data
  int T;              // Number of rows in reporting triangle 
  int D;              // Maximum delay and number of columns of reporting triangle'
  int r[T, D + 1];    // Reporting triangle (Including zero delay)
  int k_wd_haz;       // Number covariates discrete-time hazard model
  real lead_ind_1[T]; // Lead indicator 1
  real lead_ind_2[T]; // Lead indicator 2
  matrix[T, k_wd_haz] W_wd[D + 1];  // Design matrix for discrete hazard model
  matrix[T, D + 1] Z;        // Matrix indicating non-reporting days
  // prior parameter
  vector[D + 1] alpha;  // Parameters of Dirichlet prior for baseline delay distribution
}

parameters {
  simplex[D + 1] p_bl_pr; // delay probabilities
  vector[T] epsilon;   // Error log-linear model (scaled by sigma)
  // epi-curve model
  real<lower=0, upper=2> sigma; // Variance parameter for random walk
  real beta_0; // Intercept
  real beta_1; // Association coefficient 1
  real beta_2; // Association coefficient
  // reporting model
  // week-day effect
  vector[k_wd_haz] beta_wd_haz;       
  // Hyperprior
  real<lower=0> sd_beta_wd_haz;
  // data model
  real<lower=0, upper=1> reciprocal_phi;   // dispersion parameter: var=mu+reciprocal_phi*mu^2
}

transformed parameters {
  // reporting model
  vector[D] gamma;   // Discrete hazard model intercept
  vector[T] logLambda; // expected number of cases at time t in strata s
  matrix[T, D + 1] h;        // Discrete hazard w.r.t time and delay
  matrix[T, D + 1] p;        // Reporting probability w.r.t. time and delay
  
  // data model
  real phi;
  // Discrete hazard model
  gamma = logit((p_bl_pr ./ reverse(cumulative_sum(reverse(p_bl_pr))))[1:D]);
  
  for (d in 1:(D)){
    h[, d] = inv_logit(gamma[d] + W_wd[d]*beta_wd_haz) .* (rep_vector(1, T) - Z[, d]);
    if (d==1) {
        p[, d] = h[, d];
      } else {      
        p[, d] = h[, d] .* (1 - (p[, 1:(d-1)] * rep_vector(1, d-1)));
      }
  }
  h[, D + 1] = rep_vector(1, T);
  p[, D + 1] = 1 - (p[, 1:D] * rep_vector(1, D));
  // log-lambda
  for(t in 1:T){
    logLambda[t] = beta_0 + beta_1 * lead_ind_1[t] +  beta_2 * lead_ind_2[t] + sigma * epsilon[t]; // Derive logLambda from non-centered parametrization
  }
  // Overdispersion
  phi = 1 / reciprocal_phi;
}



model {
  // Priors
  // hospitalization model
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
    for (d in 0:min(T - t, D)) {
      if (p[t, d + 1] > 0){
        r[t, d + 1] ~ neg_binomial_2(exp(logLambda[t]) * p[t, d + 1], phi);
      }
    }
  }
}
 
generated quantities {
  int n[T, D + 1];
  int N[T];
  for (t in 1:T){
    for (d in 0:D){
      if (t + d <= T){
        n[t, d + 1] = r[t, d + 1];
      } else {
        if (p[t, d + 1] > 0){
          n[t, d + 1] = neg_binomial_2_rng(exp(logLambda[t]) * p[t, d + 1], phi);
        } else {
          n[t, d + 1] = 0;
        }
      }
    }
    N[t] = sum(n[t, ]);
  }
}