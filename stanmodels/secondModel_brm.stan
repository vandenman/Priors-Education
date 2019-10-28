data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> J_1[N];
  int<lower=1> N_1;
  int<lower=1> M_1;
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> J_2[N];
  int<lower=1> N_2;
  int<lower=1> M_2;
  vector[N] Z_2_1;
  int prior_only;  // should the likelihood be ignored?
  vector[5] shapes;
  vector[5] rates;
}
transformed data {
  int Kc = K - 1;
  matrix[N, K - 1] Xc;  // centered version of X
  vector[K - 1] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real temp_Intercept;  // temporary intercept
  real<lower=0> sigma;  // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // unscaled group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // unscaled group-level effects
}
transformed parameters {
  // group-level effects
  vector[N_1] r_1_1 = sd_1[1] * (z_1[1]);
  // group-level effects
  vector[N_2] r_2_1 = sd_2[1] * (z_2[1]);
}
model {
  vector[N] mu = temp_Intercept + Xc * b;
  for (n in 1:N) {
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
  }

  // informed gamma priors
  target += gamma_lpdf(sd_1  | shapes[2], rates[2]); // leerling
  target += gamma_lpdf(sd_2  | shapes[1], rates[1]); // school

  target += gamma_lpdf(sigma | shapes[4], rates[4]);
  target += gamma_lpdf(temp_Intercept | shapes[5], rates[5]);


  // priors including all constants
  target += normal_lpdf(z_1[1] | 0, 1);
  target += normal_lpdf(z_2[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = temp_Intercept - dot_product(means_X, b);
}

// data {
//   int<lower=1> N;  // total number of observations
//   vector[N] Y;  // response variable
//   int<lower=1> K;  // number of population-level effects
//   matrix[N, K] X;  // population-level design matrix
//   // data for group-level effects of ID 1
//   int<lower=1> J_1[N];
//   int<lower=1> N_1;
//   int<lower=1> M_1;
//   vector[N] Z_1_1;
//   // data for group-level effects of ID 2
//   int<lower=1> J_2[N];
//   int<lower=1> N_2;
//   int<lower=1> M_2;
//   vector[N] Z_2_1;
//   int prior_only;  // should the likelihood be ignored?
//   vector[5] shapes;
//   vector[5] rates;
// }
// transformed data {
//   int Kc = K - 1;
//   matrix[N, K - 1] Xc;  // centered version of X
//   vector[K - 1] means_X;  // column means of X before centering
//   for (i in 2:K) {
//     means_X[i - 1] = mean(X[, i]);
//     Xc[, i - 1] = X[, i] - means_X[i - 1];
//   }
// }
// parameters {
//   vector[Kc] b;  // population-level effects
//   real temp_Intercept;  // temporary intercept
//   real<lower=0> sigma;  // residual SD
//   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
//   vector[N_1] z_1[M_1];  // unscaled group-level effects
//   vector<lower=0>[M_2] sd_2;  // group-level standard deviations
//   vector[N_2] z_2[M_2];  // unscaled group-level effects
// }
// transformed parameters {
//   // group-level effects
//   vector[N_1] r_1_1 = sd_1[1] * (z_1[1]);
//   // group-level effects
//   vector[N_2] r_2_1 = sd_2[1] * (z_2[1]);
// }
// model {
//   vector[N] mu = temp_Intercept + Xc * b;
//   // print("mu = ", mu[1]);
//   for (n in 1:N) {
//     mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
//     // print("mu[", n, " = ", mu[n]);
//   }
//   // print("sigma = ", sigma);
//
//   // informed gamma priors
//   target += gamma_lpdf(sd_1  | shapes[2], rates[2]); // leerling
//   target += gamma_lpdf(sd_2  | shapes[1], rates[1]); // school
//
//   target += gamma_lpdf(sigma | shapes[4], rates[4]);
//   target += gamma_lpdf(temp_Intercept | shapes[5], rates[5]);
//
//   // priors including all constants
//   // target += student_t_lpdf(temp_Intercept | 3, 82, 18);
//   // target += student_t_lpdf(sigma | 3, 0, 18)
//   //   - 1 * student_t_lccdf(0 | 3, 0, 18);
//   // target += student_t_lpdf(sd_1 | 3, 0, 18)
//   //   - 1 * student_t_lccdf(0 | 3, 0, 18);
//   target += normal_lpdf(z_1[1] | 0, 1);
//   // target += student_t_lpdf(sd_2 | 3, 0, 18)
//   //   - 1 * student_t_lccdf(0 | 3, 0, 18);
//   target += normal_lpdf(z_2[1] | 0, 1);
//   // likelihood including all constants
//   if (!prior_only) {
//     target += normal_lpdf(Y | mu, sigma);
//   }
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = temp_Intercept - dot_product(means_X, b);
// }
