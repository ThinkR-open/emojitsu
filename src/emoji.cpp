#include <Rcpp.h>
using namespace Rcpp;

const char* sixteen = "0123456789ABCDEF" ;

int parse_digit( char d ){
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

int parse16_one( const char* spec ){
  int n = strlen(spec) ;
  int m = 1 ;

  int value = 0 ;
  for( int i=n-1; i>=0; i--, m*=16 ){
    value += m * parse_digit(spec[i]) ;
  }
  return value ;
}

// [[Rcpp::export]]
std::string encode_utf8_one( CharacterVector code_points ){
  std::string res ;
  int n = code_points.size() ;
  for(int i=0; i<n; i++){
    int point = parse16_one( code_points[i] ) ;
    if( point <= 0x7F ){
      // 1 byte
      res.push_back( point ) ;
    } else if( point <= 0x7FF ){
      // 2 bytes
      res.push_back( (point >> 6) + 192 );
      res.push_back( (point & 63) + 128 );
    } else if( point <= 0xFFFF ){
      // 3 bytes
      res.push_back( (point >> 12) + 224 );
      res.push_back( ((point >> 6 ) & 63) + 128 );
      res.push_back( (point & 63) + 128 );
    } else {
      // 4 bytes
      res.push_back( (point >> 18) + 240 );
      res.push_back( ((point >> 12 ) & 63) + 128 );
      res.push_back( ((point >> 6 ) & 63) + 128 );
      res.push_back( (point & 63) + 128 );
    }

  }
  return res ;
}

// [[Rcpp::export]]
CharacterVector encode_utf8( List code_points ){
  CharacterVector res = sapply( code_points, encode_utf8_one ) ;
  res.attr("class") = "emoji" ;
  return res ;
}
