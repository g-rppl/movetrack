// Generates state probabilities using the forward-backward algorithm

generated quantities {
  matrix[T,N] stateProbs;
  vector[N] lp;
  vector[N] lp_p1;

  // forward-backward algorithm (state probabilities)
  {
    matrix[T,N] logalpha;
    matrix[T,N] logbeta;
    matrix[T,2] mu;
    real llk;

    // log alpha probabilities
    for (i in 1:I) {
      int t1 = index[i]+1;
      int t2 = index[i+1];

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
      for (t0 in (t1+1):(t2-1)) {
        int t = (t2-1) - t0 + 2;

        if (t==t2-1) {
          for (n in 1:N)
            lp_p1[n] = 0;
        } else {
          for (n in 1:N) {
            mu[t,] = y_c[t,] + (y_c[t,] - y_c[t+1,]) * cor[i][n];
            lp_p1[n] = log_sum_exp(to_vector(log_gamma_tr[t+1,n]) + lp) + 
              multi_normal_lpdf(y_c[t-1,] | mu[t,], Omega[i]);
          }
        }
        lp = lp_p1;
        for (n in 1:N)
          logbeta[t,n] = lp[n];
      }

      // state probabilities
      for(t0 in (t1+1):(t2-1)) {
        int t = (t2-1) - t0 + 2;

        if(t==t2-1)
          llk = log_sum_exp(logalpha[t]);
        for(n in 1:N)
          stateProbs[t,n] = exp(logalpha[t,n] + logbeta[t,n] - llk);
      }
    }
  }
}