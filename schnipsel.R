
# RCPP Functions for single rms

# calculate_rolling_rm1
cppFunction('
      NumericVector calculate_rolling_rm1(NumericVector emis, double x){
      for (int i = 2; i < 83; ++i){
        emis[i] = emis[i-1] * (1 + x);
      }
      return(x);
      }
    ')

# calculate_rolling_rm2
cppFunction('
      NumericVector calculate_rolling_rm2(NumericVector emis, NumericVector year, double x, double first_year, double initial_reduction_rate, NumericVector rr){
      for (int i = 2; i < 83; ++i){
        if(year[i] == first_year) {
          rr[i] = initial_reduction_rate;
        } else {
          rr[i] = rr[i-1] * (1 + x);
        }
        emis[i] = emis[i-1] * (1 + rr[i]);
      }
      return(x);
      }
    ')

# calculate_rolling_rm3
cppFunction('
      NumericVector calculate_rolling_rm3(NumericVector emis, NumericVector year, double x, double first_year, double initial_reduction_rate, NumericVector rr){
      for (int i = 2; i < 83; ++i){
        if(year[i] == first_year) {
          rr[i] = initial_reduction_rate;
        } else {
          rr[i] = rr[i-1] + x;
        }
        emis[i] = emis[i-1] * (1 + rr[i]);
      }
      return(x);
      }
    ')

# calculate_rolling_rm4
cppFunction('
      NumericVector calculate_rolling_rm4(NumericVector emis, NumericVector year, double x, double first_year, double initial_reduction_rate, NumericVector rr){
      for (int i = 2; i < 83; ++i){
        if(year[i] == first_year) {
          rr[i] = initial_reduction_rate;
        } else {
          rr[i] = x * pow((year[i] - first_year), 2) + initial_reduction_rate;
        }
        emis[i] = emis[i-1] * (1 + rr[i]);
      }
      return(x);
      }
    ')

# calculate_rolling_rm5
cppFunction('
      NumericVector calculate_rolling_rm5(NumericVector emis, NumericVector year, double x, double first_year, double initial_reduction_rate, NumericVector rr){
      for (int i = 2; i < 83; ++i){
        if(year[i] == first_year) {
          rr[i] = initial_reduction_rate;
        } else {
          rr[i] = x * sqrt(year[i] - 0.5 - first_year) + initial_reduction_rate;
        }
        emis[i] = emis[i-1] * (1 + rr[i]);
      }
      return(x);
      }
    ')

# calculate_rolling_rm6
cppFunction('
      NumericVector calculate_rolling_rm6(NumericVector emis, double x){
      for (int i = 2; i < 83; ++i){
        emis[i] = emis[i-1] + x;
      }
      return(x);
      }
    ')