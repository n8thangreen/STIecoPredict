data {
  int<lower=0> N;
  int<lower=0> n_age;
  int<lower=0> n_ethngrp;
  // int<lower=0> n_la;
  //int<lower=0> n_metcounty.UA;
  // int<lower=0> n_gor;
  //int<lower=0> n_ONSclass;
  // int<lower=0> n_conception;
  
  int<lower=0, upper=1> male[N];
  // int<lower=0, upper=1> student[n_la];
  // int<lower=0, upper=1> IMDupperQ[n_la];
  int<lower=0, upper=n_age> age[N];
  int<lower=0, upper=n_ethngrp> ethngrp[N];
  // int<lower=0, upper=n_conception> conception[n_la];
  // int<lower=0, upper=n_gor> gor[n_la];
  //int<lower=0, upper=n_ONSclass> ONSclass[n_la];
  //int<lower=0, upper=n_metcounty.UA> metcounty.UA[n_la];
  // int<lower=0, upper=n_la> la[N];
  int<lower=0, upper=1> y[N];
}
parameters {
  // real<lower=0> sigma;
  real<lower=0> sigma_age;
  real<lower=0> sigma_ethngrp;
  // real<lower=0> sigma_la;
  //real<lower=0> sigma_ons;
  //real<lower=0> sigma_met;
  // real<lower=0> sigma_gor;
  // real<lower=0> sigma_conception;
  
  real b_0;
  real b_male;
  // real b_student;
  // real b_conception;
  // real b_IMDupperQ;
  
  vector[n_age] b_age;
  vector[n_ethngrp] b_ethngrp;
  // vector[n_gor] b_gor;
  //matrix[n_ONSclass] b_ONSclass;
  //matrix[n_metcounty.UA] b_metcount.UA;
  
  // vector[n_la] b_hat;
}
model {
  vector[N] p;
  // vector[n_la] b_la_hat;
  
  //priors
  
  b_0 ~ normal(0, 1);
  b_male ~ normal(0, 1);
  // b_student ~ normal(0, 10);
  // b_IMDupperQ ~ normal(0, 10);
  // b_conception ~ normal(0, sigma_conception);
  b_age ~ normal(0, sigma_age);
  b_ethngrp ~ normal(0, sigma_ethngrp);
  // b_gor ~ normal(0, sigma_gor);
  //b_ONSclass ~ normal(0, sigma_ons);
  //b_metcounty.UA ~ normal(0, sigma_metcounty.UA);
  
  // tau_age ~ gamma(20, 2000);
  // sigma_age = 1/sqrt(tau_age);
  sigma_age ~ uniform(0,10);

  // tau_ethngrp ~ gamma(20, 2000);
  // sigma_ethngrp = 1/sqrt(tau_ethngrp);
  sigma_ethngrp ~ uniform(0,10);


  //likelihood
  
  // for (j in 1:n_la)
  //   b_la_hat[j] = b_gor[gor[j]] + b_IMDupperQ*IMDupperQ[j] + b_student*student[j] + b_conception*conception[j]; //+ b_ONSclass[ONSclass[j]]
  // 
  // b_hat ~ normal(b_la_hat, sigma_la);
  
  for (i in 1:N)
    p[i] = fmax(0, fmin(1, inv_logit(b_0 + b_male*male[i] + b_age[age[i]] + b_ethngrp[ethngrp[i]])));// +
                                        // b_hat[la[i]])));
  y ~ bernoulli(p);
}


//TODO//
// generated quantities {
//   real y_ppc[N_TREAT, 100];
// 
//     for(v in 1:N_TREAT){
//       for (t in 1:100) {
//       y_ppc[v, t] = binomial_rng(100, inv_logit(alpha2[v] * t + alpha1[v])) / 100.0;
//       }
//     }
// 
