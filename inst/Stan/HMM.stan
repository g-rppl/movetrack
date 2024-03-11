data {
  int<lower=1> T;         // number of occasions
  int<lower=1> I;         // number of tracks
  int<lower=1> N;         // number of states
  int nCovs;              // number of covariates
  matrix[T,2] loc;        // observed location
  matrix[T,2] sigma;      // measurement error
  vector[T] w;            // proportions of time intervals
  array[I+1] int index;   // track start and end
  matrix[T,nCovs+1] covs; // covariates
}

transformed data {
  matrix[T,2] loc_c; // centred location
  
  for (i in 1:2)
    loc_c[,i] = loc[,i] - mean(loc[,i]);
}

parameters {
  matrix[T,2] y;                           // real location
  matrix[N*(N-1),nCovs+1] beta;            // transition parameters
  array[I] vector<lower=0>[2] tau;         // sd for process model
  array[I] vector<lower=0,upper=1>[N] cor; // correlation between steps
}

transformed parameters {
  matrix[T,2] y_c;                   // centred location estimates
  array[I] matrix[2,2] Omega;        // MVN diag matrix
  array[T] matrix[N,N] gamma;        // transition probabilities
  array[T] matrix[N,N] log_gamma;    // log transition probabilities
  array[T] matrix[N,N] log_gamma_tr; // transposed log transition probabilities
  
  for (i in 1:2)
    y_c[,i] = y[,i] - mean(loc[,i]);
  for (i in 1:I)
    Omega[i] = diag_matrix(tau[i]);

  // transition probabilities
  for (t in 1:T) {
    int betarow = 1;
    for (i in 1:N) {
      for (j in 1:N) {
        if (i==j) {
          gamma[t,i,j] = 1;
        } else {
          gamma[t,i,j] = exp(beta[betarow] * to_vector(covs[t]));
          betarow = betarow + 1;
        }
      }
    }
    // each row must sum to 1
    for (i in 1:N)
      log_gamma[t][i] = log(gamma[t][i] / sum(gamma[t][i]));
  }

  // transpose
  for (t in 1:T)
    for (i in 1:N)
      for (j in 1:N)
        log_gamma_tr[t,j,i] = log_gamma[t,i,j];
}

model {
  matrix[T,2] mu1;
  matrix[T,2] mu2;
  vector[N] logp;
  vector[N] logptemp;

  // PRIORS
  for (i in 1:I)
    tau[i] ~ exponential(20);

  // LIKELIHOOD
  for (i in 1:I) {
    int t1 = index[i]+1;
    int t2 = index[i+1];

    // estimate first two location
    y_c[t1,] ~ student_t(5, loc_c[t1,], sigma[t1,]);
    y_c[t1+1,] ~ multi_normal(y_c[t1,], Omega[i]);

    // PROCESS MODEL
    for (t in (t1+1):(t2-1)) {
      // initialise forward variable
      if (t==t1+1)
        logp = rep_vector(-log(N), N);

      // state-based process equation
      for (n in 1:N) {
        mu1[t,] = y_c[t,] + (y_c[t,] - y_c[t-1,]) * cor[i][n];
        logptemp[n] = log_sum_exp(to_vector(log_gamma_tr[t,n]) + logp) + 
          multi_normal_lpdf(y_c[t+1,] | mu1[t,], Omega[i]);
      }
      logp = logptemp;

      // add log forward variable to target at the end of each track
      if (t==t2-1)
        target += log_sum_exp(logp);
    }

    // OBSERVATION MODEL
    for (t in (t1+1):t2) {
      mu2[t,] = w[t] * y_c[t,] + (1 - w[t]) * y_c[t-1,];
    }
    loc_c[(t1+1):t2,1] ~ student_t(5, mu2[(t1+1):t2,1], sigma[(t1+1):t2,1]);
    loc_c[(t1+1):t2,2] ~ student_t(5, mu2[(t1+1):t2,2], sigma[(t1+1):t2,2]);
  }
}
