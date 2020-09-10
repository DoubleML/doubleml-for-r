# Function to load 401k data set
load_401k = function() {
  url = 'https://github.com/VC2015/DMLonGitHub/raw/master/sipp1991.dta'
  data = foreign::read.dta(url)
  return(data)
}

# Set up example causal PLR model with 401k example

# quickstart_example = function() {
#   data = load_401k()
  # DoubleML_PLR_401k = double_ml_data_from_data_frame(data,
  #                                         x_cols = c("age", "inc", "educ",
  #                                                     "fsize", "marr", "twoearn",
  #                                                     "db", "pira", "hown"),
  #                                         y_col = "net_tfa",
  #                                         d_cols = "e401")
#   return(DoubleML_PLR_401k)
# }
