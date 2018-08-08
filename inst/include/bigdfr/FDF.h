#ifndef FDF_H
#define FDF_H

/******************************************************************************/

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <bigdfr/utils.h>

using std::size_t;

/******************************************************************************/

class FDF : private boost::noncopyable {
public:
  FDF(std::string path,
      size_t n,
      const std::vector<size_t>& ind_row,
      const std::vector<int>& types,
      const std::vector<size_t>& column_offsets);

  void* column(size_t j) const {
    return static_cast<char*>(file_data) + column_offsets[j];
  }
  int column_type(size_t j) const { return types[j]; }

  std::vector<size_t> get_ind_row() const { return ind_row; }

  size_t nrow() const { return ind_row.size(); }
  size_t ncol() const { return types.size(); }

private:
  boost::interprocess::file_mapping file;
  boost::interprocess::mapped_region file_region;
  void* file_data;
  size_t n;
  std::vector<size_t> ind_row;
  std::vector<int> types;
  std::vector<size_t> column_offsets;
};

/******************************************************************************/

template <typename T>
class ColAcc {
public:
  ColAcc(const FDF * xpDF, size_t j) {
    _pMat = static_cast<T*>(xpDF->column(j));
    _ind_row = xpDF->get_ind_row();
  }

  inline T& operator[](size_t i) {
    return _pMat[_ind_row[i]];
  }

  size_t nrow() const { return _ind_row.size(); }

protected:
  T* _pMat;
  std::vector<size_t> _ind_row;
};

/******************************************************************************/

#endif // FDF_H
