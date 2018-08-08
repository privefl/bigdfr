/******************************************************************************/

#include <bigdfr/FDF.h>
#include <stdexcept>
#include <fstream>

using namespace Rcpp;
using namespace boost::interprocess;
using std::size_t;

/******************************************************************************/

FDF::FDF(std::string path,
         size_t n,
         const std::vector<size_t>& ind_row,
         const std::vector<int>& types,
         const std::vector<size_t>& column_offsets)
  : n(n), ind_row(ind_row), types(types), column_offsets(column_offsets) {

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
SEXP getXPtrFDF(std::string path,
                size_t n,
                IntegerVector ind_row,
                IntegerVector types) {

  // http://gallery.rcpp.org/articles/intro-to-exceptions/
  try {

    size_t m = types.size();
    size_t offset = 0;
    std::vector<int> types2(m);
    std::vector<size_t> column_offsets(m);
    for (size_t j = 0; j < m; j++) {
      column_offsets[j] = offset;
      types2[j] = types[j];
      offset += n * types[j];
    }

    size_t n_part = ind_row.size();
    std::vector<size_t> ind_row2(n_part);
    for (size_t i = 0; i < n_part; i++) {
      ind_row2[i] = ind_row[i] - 1;
    }

    // Create a pointer to an FDF object and wrap it as an external pointer
    XPtr<FDF> ptr(new FDF(path, n, ind_row2, types2, column_offsets), true);
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

// [[Rcpp::export]]
void add_bytes(std::string fileName, std::size_t nbyte) {

  myassert(nbyte > 0, "Dimensions should be at least 1.");

  try {

    std::fstream filestr(fileName.c_str());
    if (filestr) {
      std::streambuf* pbuf = filestr.rdbuf();
      pbuf->pubseekoff(nbyte - 1, filestr.end);
      pbuf->sputc(0);
      filestr.close();
    }

  } catch(std::exception& ex) {
    throw std::runtime_error("Problem resizing the backing file.");
  }
}

/******************************************************************************/
