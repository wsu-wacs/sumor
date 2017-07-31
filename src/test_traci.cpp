#include <Rcpp.h>
#include <iostream>
#include "TraCIAPI.h"

using namespace Rcpp;

class Client : public TraCIAPI {
public:
  Client() {};
  ~Client() {};
};

// [[Rcpp::export()]]
void test_traci(){
  Client client;
  client.connect("localhost", 1337);
  Rcout << "time in ms: " << client.simulation.getCurrentTime() << "\n";
  Rcout << "run 5 steps ...\n";
  client.simulationStep(5 * 1000);
  Rcout << "time in ms: " << client.simulation.getCurrentTime() << "\n";
  client.close();
}


/*** R
test_traci()
*/
