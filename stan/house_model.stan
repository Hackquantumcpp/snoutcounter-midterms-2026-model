data {
    int <lower=0> N; // number of observations
    vector[N] dem_2p_pct; // Democratic 2-party vote percentage
    vector[N] pvi; // PVI
    vector[N] generic_ballot_avg; // Generic ballot average
    // vector[N] generic_ballot_std; // Generic ballot standard deviation
    vector[N] dem_funds_2p_pct_sqrd; // Democratic 2-party share of individual contributions squared
    vector[N] dem_inc; // Democratic incumbency status (0/1)
    vector[N] rep_inc; // Republican incumbency status (0/1)
    vector[N] sqrt_effn; // Square root of effective number of polls
    vector[N] poll_margin; // Poll margin (Republican - Democratic)
    vector[N] dem_scandal_score; // Democratic scandal score
    vector[N] rep_scandal_score; // Republican scandal score

    // Random group-level effects
    vector[N] year; // Year of election cycle
    vector[N] state; // State identifier
    vector[N] census_region; // Census region identifier
    vector[N] dem_cand; // Democratic candidate
    vector[N] rep_cand; // Republican candidate
    vector[N] demo_cluster; // Demographic k-means cluster identifier

}

parameters {
    real alpha; // Intercept
    real beta_0; // Coeff for PVI
    real beta_1; // Coeff for generic ballot
    real beta_2; // Coeff for dem_funds_2p_pct_sqrd
    real beta_3; // Coeff for dem_inc
    real beta_4; // Coeff for rep_inc
    real beta_5; // Coeff for sqrt_effn:poll_margin interaction
    real beta_6; // Coeff for dem_scandal_score
    real beta_7; // Coeff for rep_scandal_score

}

model {
    
}