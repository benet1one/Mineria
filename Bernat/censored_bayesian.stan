
data {
    // Separate popularity between (popularity == 0) and (popularity > 0)
    int<lower=0> N;
    int<lower=0> N_pos;
    int<lower=0> N_zero;
    array[N_pos]  int index_pos;
    array[N_zero] int index_zero;
    array[N] real<lower=0, upper=100> popularity;
    
    // Design Matrix
    int<lower=0> k_numeric;
    int<lower=0> k_categorical;
    matrix[N, k_numeric] x_numeric;
    matrix[N, k_categorical] x_categorical;
}
parameters {
    // Optimal x
    vector<lower=-3, upper=+3>[k_numeric] optimal_x;
    // Coefficients
    real constant;
    vector<lower=0>[k_numeric] beta;
    vector[k_categorical] gamma;
    
    // Standard deviation function coefficients.
    real<lower=0.01> s0;
    real<lower=0.01> s1;
}
model {
    // Optimal x prior and x transformation
    optimal_x ~ normal(0, 1);
    
    matrix[N, k_numeric] x_transformed;
    for (j in 1:k_numeric) {
        vector[N] xj = x_numeric[, j];
        real oj = optimal_x[j];
        x_transformed[, j] = (xj - oj).^2;
    }
    
    // Expected values
    vector[N] mu = constant + (x_categorical * gamma) - (x_numeric * beta);
    // Standard deviation function.
    vector[N] sigma = s0 + s1 * fmax(mu, 0);
    
    // Positive
    popularity[index_pos] ~ normal(mu[index_pos], sigma[index_pos]);
    // Left Censored
    target += normal_lcdf(0 | mu[index_zero], sigma[index_zero]);
}
