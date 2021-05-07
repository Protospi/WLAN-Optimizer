#include <Rcpp.h>
using namespace Rcpp;

double square(double x){
  double squared = x * x;
  return squared;
}


std::vector<double> conta_pontos(double x, double y, DoubleVector wlanx, DoubleVector wlany, NumericVector indice) {
  
  // Declara tamanho de pontos
  int n = wlany.size();
  double soma;
  double raio = square(85);
  
  // Declara vetor de indices
  std::vector<double> indices(0);
  
  // Laco para condicao de pertencimento
  for(int i = 0; i < n; i++) {
    
    // Calcula soma de quadrados
    soma = square(wlanx[i] - x) + square(wlany[i] - y);
    
    // Condicao de pertencimento
    if( soma <= raio) {
      
      // Popula indices
      indices.push_back(indice[i]);
      
    }
    
  }
  
  // Retorno da funcao
  return indices;
}


// [[Rcpp::export]]
List rastreia(int intervalo_x, int intervalo_y, DoubleVector wlanx, DoubleVector wlany, DoubleVector centro_x, DoubleVector centro_y, NumericVector indice, int mais_populoso, std::vector<int> viola) {
  
  std::vector<double> cobertura(0);
  List ret;
  int intervalos = intervalo_x * intervalo_y; 
  
  for (int j = 2; j <= intervalos; j++) {
    
    if( (conta_pontos(centro_x[j],
                      centro_y[j],
                              wlanx,
                              wlany,
                              indice).size() >= 
                                conta_pontos(centro_x[mais_populoso],
                                             centro_y[mais_populoso],
                                                     wlanx,
                                                     wlany,
                                                     indice).size() ) &&
                                                       (! std::count(viola.begin(), viola.end(), j) ) ){
      
      mais_populoso = j;
      
      cobertura = conta_pontos(centro_x[mais_populoso],
                               centro_y[mais_populoso],
                                       wlanx,
                                       wlany,
                                       indice);
    }
    
    ret["cobertura"] = cobertura;
    ret["mais_populoso"] = mais_populoso;
    
  }
  
  return ret;
  
}