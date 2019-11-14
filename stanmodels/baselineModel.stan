// generated with brms 2.10.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  // temporary intercept for centered predictors
  real Intercept;
  real<lower=0> sigma;  // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  // standardized group-level effects
  vector[N_1] z_1[M_1];
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  // standardized group-level effects
  vector[N_2] z_2[M_2];
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  // standardized group-level effects
  vector[N_3] z_3[M_3];
}
transformed parameters {
  // actual group-level effects
  vector[N_1] r_1_1 = (sd_1[1] * (z_1[1]));
  // actual group-level effects
  vector[N_2] r_2_1 = (sd_2[1] * (z_2[1]));
  // actual group-level effects
  vector[N_3] r_3_1 = (sd_3[1] * (z_3[1]));
}
model {
  // initialize linear predictor term
  vector[N] mu = Intercept + Xc * b;
  for (n in 1:N) {
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n];
  }
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 90, 18);
  target += student_t_lpdf(sigma | 3, 0, 18)
    - 1 * student_t_lccdf(0 | 3, 0, 18);
  target += student_t_lpdf(sd_1 | 3, 0, 18)
    - 1 * student_t_lccdf(0 | 3, 0, 18);
  target += normal_lpdf(z_1[1] | 0, 1);
  target += student_t_lpdf(sd_2 | 3, 0, 18)
    - 1 * student_t_lccdf(0 | 3, 0, 18);
  target += normal_lpdf(z_2[1] | 0, 1);
  target += student_t_lpdf(sd_3 | 3, 0, 18)
    - 1 * student_t_lccdf(0 | 3, 0, 18);
  target += normal_lpdf(z_3[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}
