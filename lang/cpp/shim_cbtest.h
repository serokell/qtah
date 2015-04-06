#ifndef SHIM_CBTEST_H
#define SHIM_CBTEST_H

#include <string>
#include "callback.h"

namespace qtpi {

void shim_cbtest_set(cppop::Callback callback);

std::string shim_cbtest_call(const std::string& str);

}

#endif // SHIM_CBTEST_H
