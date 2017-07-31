#include <Rcpp.h>
using namespace Rcpp;

const char* sixteen = "0123456789abcdef" ;

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
  case 'a':
  case 'A':
    return 10 ;
  case 'b':
  case 'B':
    return 11 ;
  case 'c':
  case 'C':
    return 12 ;
  case 'd':
  case 'D':
    return 13 ;
  case 'e':
  case 'E':
    return 14 ;
  case 'f':
  case 'F':
    return 15 ;
  default: break ;
  }
  /* should not happen */
  return 0 ;
}

CharacterVector split_code_points( CharacterVector code_points ){
  return code_points ;
}

List split_all_codepoints( List code_points ){
  return lapply( code_points, split_code_points ) ;
}

// //' @export
// // [[Rcpp::export]]
// int parse16_one( const char* spec ){
//   int n = strlen(spec) ;
//   int m = 1 ;
//
//   int value = 0 ;
//   for( int i=n-1; i>=0; i--, m*=16 ){
//     value += m*parse_digit(spec[i]) ;
//   }
//   return value ;
// }
//
// const char* parseCodePoint( CharacterVector s ){
//   IntegerVector parts = sapply( s, parse16_one ) ;
//   int n = s.size() ;
//
//   std::string out ;
//   for(int i=0; i<n; i++){
//     // int x = parse16_one( s[i] ) ;
//
//   }
//   return "" ;
//
// }
//
//
//
//
// //' @export
// // [[Rcpp::export]]
// IntegerVector fromCodePoint( const char* s){
//   int codePoint = parse16_one(s) ;
//   if( codePoint <= 0xFFFF ){
//     return IntegerVector::create(codePoint) ;
//   } else {
//     codePoint -= 0x10000 ;
//     int highSurrogate = (codePoint >> 10) + 0xD800;
//     int lowSurrogate = (codePoint % 0x400) + 0xDC00;
//     return IntegerVector::create(highSurrogate,lowSurrogate  ) ;
//   }
// }
//
// //' @export
// // [[Rcpp::export]]
// const char* fromCharCode(int code){
//   std::string out(4, ' ') ;
//
//   out[0] = sixteen[ ( code & 0xF000 ) / 4096 ] ;
//   out[1] = sixteen[ ( code & 0x0F00 ) / 256  ] ;
//   out[2] = sixteen[ ( code & 0x00F0 ) / 16   ] ;
//   out[3] = sixteen[   code & 0x000F          ] ;
//   return out.c_str() ;
// }



