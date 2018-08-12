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
      const std::vector<int>& types,
      const std::vector<size_t>& column_offsets);

  void* column(size_t j) const {
    return static_cast<char*>(file_data) + column_offsets[j];
  }

  int column_type(size_t j) const { return types[j]; }

private:
  boost::interprocess::file_mapping file;
  boost::interprocess::mapped_region file_region;
  void* file_data;
  std::vector<int> types;
  std::vector<size_t> column_offsets;
};

/******************************************************************************/

template <typename T>
class ColAcc {
public:
  ColAcc(const FDF * xpDF, size_t j) {
    _pMat    = static_cast<T*>(xpDF->column(j));
    myassert(xpDF->column_type(j) == sizeof(T), ERROR_REPORT);
  }

  inline T& operator[](int i) {
    return _pMat[i];
  }

protected:
  T* _pMat;
};

/******************************************************************************/

#endif // FDF_H
