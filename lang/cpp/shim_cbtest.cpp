#include "shim_cbtest.h"

#include "buffer.h"
#include "common.h"

namespace {

static cppop::scoped_ptr<cppop::Callback> callback_;

}

namespace qtpi {

void shim_cbtest_set(cppop::Callback callback) {
    callback_.assign(new cppop::Callback(callback));
}

std::string shim_cbtest_call(const std::string& str) {
    cppop::scoped_ptr<cppop::SizedBuffer> args(new cppop::SizedBuffer());
    cppop::SizedBufferWriter argsWriter(*args);
    cppop::encodeStdString(str + "(in C++)", argsWriter);
    cppop::SizedBuffer response;
    callback_->invoke(args.release(), response);
    cppop::SizedBufferReader responseReader(response);
    std::string str2;
    str2 = cppop::decodeStdString(*(cppop::Server*)NULL, responseReader);
    return str2 + "(back in C++)";
}

}
