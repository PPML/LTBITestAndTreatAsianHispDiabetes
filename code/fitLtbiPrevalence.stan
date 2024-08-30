stan_code <- "
data {
  // INPUTS
  int         N; 
  matrix[N,2] pri_test; 
  real        pri_sens[2];
  real        pri_spec[2];
}
///////////////////////////////////////////////////////////
transformed data {
}
///////////////////////////////////////////////////////////
parameters {
  vector<lower=0,upper=1>[N]  ltbi;
  real<lower=0,upper=1>       sens;
  real<lower=0,upper=1>       spec;
}
///////////////////////////////////////////  <lower = -1, upper = 1>
transformed parameters {
///~~~~~~~ Define ~~~~~~~
  vector[N] test;

 ///~~~~~~~ Calcs ~~~~~~~
  for(i in 1:N){
    test[i] = ltbi[i]*sens + (1-ltbi[i])*(1-spec);
  }
 

}
///////////////////////////////////////////////////////////  
model {
 ///~~~~~~~ PSEUDO-LIKELIHOOD ~~~~~~~
  for(i in 1:N){
    test[i]  ~ beta(pri_test[i,1], pri_test[i,2]);
  }
 ///~~~~~~~ PRIORS ~~~~~~~
  sens  ~ beta(pri_sens[1], pri_sens[2]);
  spec  ~ beta(pri_spec[1], pri_spec[2]);
  ltbi  ~ beta(1.00,1.00);
}
///////////////////////////////////////////////////////////
generated quantities {

}
"

