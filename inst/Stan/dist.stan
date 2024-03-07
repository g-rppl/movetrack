functions {
  real distGeo(row_vector p1, row_vector p2) {
    real dLon = (p2[1] - p1[1]) * pi() / 180;
    real dLat = (p2[2] - p1[2]) * pi() / 180;

    real lat1 = p1[2] * pi() / 180;
    real lat2 = p2[2] * pi() / 180;

    real a = sin(dLat / 2) * sin(dLat / 2) +
             sin(dLon / 2) * sin(dLon / 2) * cos(lat1) * cos(lat2);
    real c = 2 * atan2(sqrt(a), sqrt(1 - a));

    return 6378.137 * c;
  }
}

data {
  int<lower=3> N;    // number of occasions
  matrix[N,2] loc;   // observed location
  matrix[N,2] sigma; // measurement error
  vector[N] w;       // proportions of time intervals
  vector<lower=0>[N-1] max_dist; // maximum speed
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
  vector[N-1] dist;
  
  for (i in 1:2)
    y_c[,i] = y[,i] - mean(loc[,i]);
  
  Omega = diag_matrix(tau);

  for (t in 2:N-1)
    dist[t] = distGeo(y[t,], y[t-1,]);
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
      mu1[t,] = y_c[t,] + dist[t] * gamma;
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
