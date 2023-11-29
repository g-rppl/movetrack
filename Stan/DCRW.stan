data {
  int<lower=3> N;    // number of occasions
  matrix[N,2] loc;   // observed location
  matrix[N,2] sigma; // measurement error
  vector[N] w;       // proportions of time intervals
}

transformed data {
  matrix[N,2] loc_c; // centred location
  
  for (i in 1:2)
    loc_c[,i] = loc[,i] - mean(loc[,i]);
}

parameters {
  matrix[N,2] y;                // real location
  vector<lower=0>[2] tau;       // sd for process model
  real<lower=0,upper=1> gamma;  // correlation between steps
}

transformed parameters {
  matrix[N,2] y_c;   // centred location estimate
  matrix[2,2] Omega; // MVN diag matrix
  
  for (i in 1:2)
    y_c[,i] = y[,i] - mean(loc[,i]);
  
  Omega = diag_matrix(tau);
}

model {
  // PRIORS
  tau ~ exponential(20);
  
  // PROCESS MODEL
  // estimate first two location
  y_c[1,] ~ student_t(5, loc_c[1,], sigma[1,]);
  y_c[2,] ~ multi_normal(y_c[1,], Omega);
  
  // process equation
  {
    matrix[N,2] mu1;
    for (t in 2:N-1){
      mu1[t,] = y_c[t,] + (y_c[t,] - y_c[t-1,]) * gamma;
      y_c[t+1,] ~ multi_normal(mu1[t,], Omega);
    }
  }
  
  // OBSERVATION MODEL
  {
    matrix[N,2] mu2;
    for (t in 2:N){
      mu2[t,] = w[t] * y_c[t,] + (1 - w[t]) * y_c[t-1,];
    }
    loc_c[2:N,1] ~ student_t(5, mu2[2:N,1], sigma[2:N,1]);
    loc_c[2:N,2] ~ student_t(5, mu2[2:N,2], sigma[2:N,2]);
  }
}
