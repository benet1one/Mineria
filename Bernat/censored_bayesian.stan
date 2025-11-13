
data {
    // Separate popularity between (popularity == 0) and (popularity > 0)
    int<lower=0> N;
    int<lower=0> N_pos;
    int<lower=0> N_zero;
    array[N_pos]  int index_pos;
    array[N_zero] int index_zero;
    array[N] real<lower=0, upper=100> popularity;
    
    // Design Matrix
    int<lower=0> k;
    matrix[N, k] x;
}
parameters {
    // Coeficients
    vector[k] beta;
    
    // Deviation function coefficients.
    real<lower=0.01> s0;
    real<lower=0.01> s1;
}
model {
    // Expected values
    vector[N] mu = x * beta;
    // Standard deviation
    vector[N] sigma = s0 + s1 * fmax(mu, 0);
    
    // Positive
    popularity[index_pos] ~ normal(mu[index_pos], sigma[index_pos]);
    // Left Censored
    target += normal_lcdf(0 | mu[index_zero], sigma[index_zero]);
}
