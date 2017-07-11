#include <Rcpp.h>
using namespace Rcpp;

const char* sixteen = "0123456789ABCDEF" ;

int parse_digit( char d){
  switch(d){
  case '0': return 0 ;
  case '1': return 1 ;
  case '2': return 2 ;
  case '3': return 3 ;
  case '4': return 4 ;
  case '5': return 5 ;
  case '6': return 6 ;
  case '7': return 7 ;
  case '8': return 8 ;
  case '9': return 9 ;
  case 'A': return 10 ;
  case 'B': return 11 ;
  case 'C': return 12 ;
  case 'D': return 13 ;
  case 'E': return 14 ;
  case 'F': return 15 ;
  default: break ;
  }
  /* should not happen */
  return 0 ;
}

//' @export
// [[Rcpp::export]]
int parse16_one( const char* spec ){
  int n = strlen(spec) ;
  int m = 1 ;

  int value = 0 ;
  for( int i=n-1; i>=0; i--, m*=16 ){
    value += m*parse_digit(spec[i]) ;
  }
  return value ;
}


IntegerVector parse16_range( CharacterVector spec ){
  if( spec.size() == 1){
    return IntegerVector::create( parse16_one(spec[0]) ) ;
  } else {
    return seq( parse16_one(spec[0]), parse16_one(spec[1]) ) ;
  }

}

//' @export
// [[Rcpp::export]]
List parse16( List spec ) {
  int n = spec.size() ;
  List res = lapply( spec, parse16_range ) ;
  return res ;
}

//' @export
// [[Rcpp::export]]
IntegerVector fromCodePoint( const char* s){
  int codePoint = parse16_one(s) ;
  if( codePoint <= 0xFFFF ){
    return IntegerVector::create(codePoint) ;
  } else {
    codePoint -= 0x10000 ;
    int highSurrogate = (codePoint >> 10) + 0xD800;
    int lowSurrogate = (codePoint % 0x400) + 0xDC00;
    return IntegerVector::create(highSurrogate,lowSurrogate  ) ;
  }
}

