load_401k = function(){
  url = 'https://github.com/VC2015/DMLonGitHub/raw/master/sipp1991.dta'
  data = foreign::read.dta(url)
  return(data)
}