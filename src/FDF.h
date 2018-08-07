#ifndef FDF_H
#define FDF_H

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <Rcpp.h>

using std::size_t;

class FDF : private boost::noncopyable {
public:
  FDF(std::string path, size_t n, std::vector<int> types);

  void* column(size_t j) const {
    return static_cast<char*>(file_data) +
      n * std::accumulate(types.begin(), types.begin() + j, 0);
  }
  size_t nrow() const { return n; }
  size_t ncol() const { return types.size(); }
  int column_type(size_t j) const { return types[j]; }

private:
  boost::interprocess::file_mapping file;
  boost::interprocess::mapped_region file_region;
  void* file_data;
  size_t n;
  std::vector<int> types;
};

#endif // FDF_H
