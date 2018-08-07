/******************************************************************************/

#include "FDF.h"

using namespace Rcpp;
using namespace boost::interprocess;
using std::size_t;

/******************************************************************************/

FDF::FDF(std::string path, size_t n, std::vector<int> types)
  : n(n), types(types) {

  try {
    this->file = file_mapping(path.c_str(), read_write);
  } catch(interprocess_exception& e) {
    throw std::runtime_error("File '" + path + "' not found.");
  }

  this->file_region = mapped_region(this->file, read_write);
  this->file_data = reinterpret_cast<void*>(this->file_region.get_address());

  // const size_t num_bytes = this->file_region.get_size();
  // Rcout << "FDF: " << (int)this->n << " " << (int)this->m << std::endl; //DEBUG
}

/******************************************************************************/

// [[Rcpp::export]]
SEXP getXPtrFDF(std::string path, size_t n, IntegerVector types) {

  // http://gallery.rcpp.org/articles/intro-to-exceptions/
  try {
    // Create a pointer to an FDF object and wrap it as an external pointer
    std::vector<int> types2(types.size());
    std::copy(types.begin(), types.end(), types2.begin());
    XPtr<FDF> ptr(new FDF(path, n, types2), true);
    // Return the external pointer to the R side
    return ptr;
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...) {
    ::Rf_error("C++ exception (unknown reason)");
  }

  return R_NilValue;
}

/******************************************************************************/

