#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// http://www.cplusplus.com/reference/unordered_map/unordered_map/reserve/
// unordered_map::reserve
#include <iostream>
#include <string>
#include <unordered_map>

// [[Rcpp::export]]
int test_unordered_map()
{
  std::unordered_map<std::string, std::string> mymap;

  mymap.reserve(6);

  mymap["house"] = "maison";
  mymap["apple"] = "pomme";
  mymap["tree"] = "arbre";
  mymap["book"] = "livre";
  mymap["door"] = "porte";
  mymap["grapefruit"] = "pamplemousse";

  for (auto& x: mymap) {
    Rcout << x.first << ": " << x.second << std::endl;
  }

  return 0;
}


/*** R
test_unordered_map()
*/
