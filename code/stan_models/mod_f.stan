// Model description:
// Perform Bayesian hierarchical Nowcast with log-linear model for 
// log(lambda_{t}) = N(beta_0 * log(lambda_{t-1}) + beta_1 * lead_ind_{t}), sigma), 
// model observed case counts n_{t,d} ~ NB(lambda[t]*p_{t,d}, phi), 
// with phi over-dispersion
// Delay distribution: Discrete time-hazard model with week-day effects
data {
  // Data
  int T;              // Number of rows in reporting triangle 
  int D;              // Maximum delay and number of columns of reporting triangle'
  int r[T, D + 1];    // Reporting triangle (Including zero delay)
  real lead_ind[T];   // Lead indicator
  int k_wd_haz;       // Number covariates discrete-time hazard model
  matrix[T, k_wd_haz] W_wd[D + 1];  // Design matrix for discrete hazard model
  matrix[T, D + 1] Z;        // Matrix indicating non-reporting days
  // prior parameter
  vector[D + 1] alpha;  // Parameters of Dirichlet prior for baseline delay distribution
}

parameters {
  simplex[D + 1] p_bl_pr; // delay probabilities
  vector[T] epsilon;   // Error log-linear model (scaled by sigma)
  vector[T] logLambda; // expected number of cases at time t in strata s
  // epi-curve model
  real<lower=0, upper=2> sigma; // Variance parameter for random walk
  real<lower=-2, upper=2> beta_1; // Association coefficient
  // reporting model
  // week-day effect
  vector[k_wd_haz] beta_wd_haz;       
  // Hyperprior
  real<lower=0> sd_beta_wd_haz;
  // Data model
  real<lower=0, upper=1> reciprocal_phi;   // dispersion parameter: var=mu+reciprocal_phi*mu^2
}

transformed parameters {
  // eporting model
  vector[D] gamma;   // Discrete hazard model intercept
  matrix[T, D + 1] h;        // Discrete hazard w.r.t time and delay
  matrix[T, D + 1] p;        // Reporting probability w.r.t. time and delay
  
  // data model
  real phi;
  // Discrete hazard model
  gamma = logit((p_bl_pr ./ cumulative_sum(p_bl_pr[(D + 1):1])[(D + 1):1])[1:(D)]);
  
  for (d in 1:(D)){
    h[, d] = inv_logit(gamma[d] + W_wd[d]*beta_wd_haz) .* (rep_vector(1, T) - Z[, d]);
    if (d==1) {
        p[, d] = h[, d];
      } else {      
        p[, d] = h[, d] .* (1 - (p[, 1:(d-1)] * rep_vector(1, d-1)));
      }
  }
  h[, D + 1] = rep_vector(1, T);
  p[, D + 1] = 1 -  (p[, 1:(D)] * rep_vector(1, D));
  // Overdispersion
  phi = 1 / reciprocal_phi;
}

model {
  // Priors
  sigma ~ normal(0, .5); // scale of the error-term
  beta_1 ~  normal(0, .1);
  // Random walk
  logLambda[1] ~ normal(log(fmax(sum(r[1,]),1)), .1);
  for(t in 2:T) {
    logLambda[t] ~ normal(log(fmax(exp(logLambda[t-1]) + beta_1 * lead_ind[t-1],1)), sigma);
  }
  // Reporting delay
  // Hyper-prior
  p_bl_pr ~ dirichlet(alpha);
  sd_beta_wd_haz ~ normal(0, 0.5);
  // Prior
  beta_wd_haz ~ normal(0, sd_beta_wd_haz);

  // log-Lambda
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
          n[t, d +1] = 0;
        }
      }
    }
    N[t] = sum(n[t, ]);
  }
}