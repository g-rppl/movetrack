data {
  int<lower=1> T;         // number of occasions
  int<lower=1> I;         // number of tracks
  int<lower=1> N;         // number of states
  int nCovs;              // number of covariates
  matrix[T,2] loc;        // observed location
  matrix[T,2] sigma;      // measurement error
  array[I,2] int index;   // track start and end
  matrix[T,nCovs+1] covs; // covariates
  vector[T] w;            // proportions of time intervals
}

transformed data {
  matrix[T,2] loc_c; // centred location
  
  for (i in 1:2)
    loc_c[,i] = loc[,i] - mean(loc[,i]);
}

parameters {
  matrix[T,2] y;                  // real location
  matrix[N*(N-1),nCovs+1] beta;   // transition parameters
  array[I] vector<lower=0>[2] tau;         // sd for process model
  array[I] vector<lower=0,upper=1>[N] cor; // correlation between steps
}

transformed parameters {
  matrix[T,2] y_c;   // centred location estimates
  array[I] matrix[2,2] Omega; // MVN diag matrix
  
  for (i in 1:2)
    y_c[,i] = y[,i] - mean(loc[,i]);
  for (i in 1:I)
    Omega[i] = diag_matrix(tau[i]);
}

model {
  matrix[T,2] mu1;
  matrix[T,2] mu2;
  vector[N] logp;
  vector[N] logptemp;
  array[T] matrix[N,N] gamma;
  array[T] matrix[N,N] log_gamma;
  array[T] matrix[N,N] log_gamma_tr;

  // PRIORS
  for (i in 1:I)
    tau[i] ~ exponential(20);

  // Transition probabilities
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

  // Transpose
  for (t in 1:T)
    for (i in 1:N)
      for (j in 1:N)
        log_gamma_tr[t,j,i] = log_gamma[t,i,j];

  for (i in 1:I) {
    int t1 = index[i,1];
    int t2 = index[i,2];

    // PROCESS MODEL
    // estimate first two location
    y_c[t1,] ~ student_t(5, loc_c[t1,], sigma[t1,]);
    y_c[t1+1,] ~ multi_normal(y_c[t1,], Omega[i]);
    for (t in (t1+1):(t2-1)) {
      // initialise forward variable
      if (t==t1+1)
        logp = rep_vector(-log(N), N);

      // State-based process equation
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

generated quantities {
  // int<lower=1,upper=N> viterbi[T];
  matrix[T,N] stateProbs;
  vector[N] lp;
  vector[N] lp_p1;

  // Viterbi algorithm (most likely state sequence)
  // {
  //   real max_logp;
  //   int back_ptr[T, N];
  //   real best_logp[T, N];

  //   for (t in 1:T) {
  //     if(t==1 || ID[t]!=ID[t-1]) {
  //       for(n in 1:N)
  //         best_logp[t, n] = gamma_lpdf(steps[t] | shape[n], rate[n]);
  //     } else {
  //     for (n in 1:N) {
  //       best_logp[t, n] = negative_infinity();
  //         for (j in 1:N) {
  //           real logp;
  //           logp = best_logp[t-1, j] + log_theta[t,j,n];
  //           if(steps[t]>0)
  //             logp = logp + gamma_lpdf(steps[t] | shape[n], rate[n]);
  //           if(angles[t]>(-pi()))
  //             logp = logp + von_mises_lpdf(angles[t] | loc[n], kappa[n]);
  //           if (logp > best_logp[t, n]) {
  //             back_ptr[t, n] = j;
  //             best_logp[t, n] = logp;
  //           }
  //         }
  //       }
  //     }
  //   }
  //   for(t0 in 1:T) {
  //     int t = T - t0 + 1;
  //     if(t==T || ID[t+1]!=ID[t]) {
  //       max_logp = max(best_logp[t]);
  //       for (n in 1:N)
  //         if (best_logp[t, n] == max_logp)
  //           viterbi[t] = n;
  //     } else {
  //       viterbi[t] = back_ptr[t+1, viterbi[t+1]];
  //     }
  //   }
  // }

  // forward-backward algorithm (state probabilities)
  {
    matrix[T,N] logalpha;
    matrix[T,N] logbeta;
    real llk;
    matrix[T,2] mu;

    // log alpha probabilities
    for (i in 1:I) {
      int t1 = index[i,1];
      int t2 = index[i,2];

      for(t in (t1+1):(t2-1)) {
        if(t==t1+1) {
          for(n in 1:N)
            lp[n] = -log(N);
        }
        for (n in 1:N) {
          mu[t,] = y_c[t,] + (y_c[t,] - y_c[t-1,]) * cor[i][n];
          lp_p1[n] = log_sum_exp(to_vector(log_gamma_tr[t,n]) + lp) + 
          multi_normal_lpdf(y_c[t+1,] | mu[t,], Omega[i]);
          logalpha[t,n] = lp_p1[n];
        }
        lp = lp_p1;
      }

      // log beta probabilities
      for(t0 in (t1+1):(t2-1)) {
        int t = t2 - t0 + 1;
        if(t==t2-1) {
          for(n in 1:N)
            lp_p1[n] = 0;
        } else {
          for(n in 1:N) {
            mu[t,] = y_c[t,] + (y_c[t,] - y_c[t-1,]) * cor[i][n];
            lp_p1[n] = log_sum_exp(to_vector(log_gamma_tr[t,n]) + lp) + 
            multi_normal_lpdf(y_c[t+1,] | mu[t,], Omega[i]);
          }
        }
        lp = lp_p1;
        for(n in 1:N)
          logbeta[t,n] = lp[n];
      }

      // state probabilities
      for(t0 in (t1+1):(t2-1)) {
        int t = t2 - t0 + 1;
        if(t==t2)
          llk = log_sum_exp(logalpha[t]);
        for(n in 1:N)
          stateProbs[t,n] = exp(logalpha[t,n] + logbeta[t,n] - llk);
      }
    }
  }
}
