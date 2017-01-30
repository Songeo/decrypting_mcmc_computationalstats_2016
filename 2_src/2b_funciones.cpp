#include <Rcpp.h>
using namespace Rcpp; 


// [[Rcpp::export]]
IntegerVector Decode(IntegerVector abcd, IntegerVector cadenacod){
  IntegerVector resultados(cadenacod.length());
  
  for (int i=0; i < cadenacod.length(); i++) {
    int pos = cadenacod[i]-1;
    if(pos < 32){
      resultados[i] = abcd[pos];
    }else{
      resultados[i] = 33;
    }
    
  }
  return resultados;
}



// [[Rcpp::export]]
double LogProb(IntegerVector cadenacod, 
               NumericMatrix mattrans){
  
  double logprob = 0;
  int anterior = 33;
  int actual;
  
  for (int i=0; i < cadenacod.length(); i++) {
    actual = cadenacod[i]-1;

    if(actual < 32){
      logprob = logprob + log( mattrans(anterior, actual) );
      anterior = actual;
    }else{
      logprob = logprob + log( mattrans(anterior, 32) );
      anterior = 32;
    }
    
  }

  if(anterior < 32){
    logprob = logprob + log( mattrans(anterior, 32) );
  }

  return logprob;
}


// [[Rcpp::export]]
int NumAletorio(){
  int randNum = rand()%(1-34 + 1) + 1;
  return randNum;
}


// [[Rcpp::export]]
DoubleVector Decripi(IntegerVector abcd,
               IntegerVector cadenacod,
               NumericMatrix mattrans,
               int niters){


  IntegerVector actualdecode = Decode(abcd, cadenacod);
  IntegerVector propdecode(cadenacod.length());

  DoubleVector maxloglike(niters + 1);

  IntegerVector propabcd(abcd.length());
  IntegerVector propuesta(2);

  double proploglike;
  double actualloglike = LogProb(actualdecode, mattrans);
  maxloglike[0] = actualloglike;
  int i = 0;

  while(i <= niters){
    
    propuesta[0] = NumAletorio();
    propuesta[1] = NumAletorio();
    // diferentes propuestas
    while(propuesta[0] == propuesta[1]){
      propuesta[1] = NumAletorio();
    }

    propabcd = abcd;
    propabcd[propuesta[0]] = abcd[propuesta[1]];
    propabcd[propuesta[1]] = abcd[propuesta[0]];

    propdecode = Decode(propabcd, cadenacod);
    proploglike = LogProb(propdecode, mattrans);
    
    double u = runif(1)[0];
    if( u < exp(proploglike - actualloglike)){
      abcd = propabcd;
      actualdecode = propdecode;
      actualloglike = proploglike;

      // loglikelihood máxima
      if(actualloglike > maxloglike[i]){
        maxloglike[i+1] = actualloglike;
      }else{
        maxloglike[i+1] = maxloglike[i];
      }
    } //if de condiciones
    i = i+1;
  } // cierra while

  return maxloglike;
}


// [[Rcpp::export]]
NumericVector unifRandTest(int n) {
  Rcpp::NumericVector x(n);
  for (int i=0; i<n; i++) {
    x[i] = unif_rand();
  }
  return x;
}


// [[Rcpp::export]]
NumericVector unifRunifTest(int n) {
  Rcpp::NumericVector x(n);
  for (int i=0; i<n; i++) {
    x[i] = runif(1)[0];
  }
  return x;
}