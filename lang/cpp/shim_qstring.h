#ifndef SHIM_QSTRING_H
#define SHIM_QSTRING_H

#include <QString>
#include "buffer.h"

namespace cppop {

class Server;

}

namespace qtpi {

QString decodeQString(::cppop::Server& server, ::cppop::SizedBufferReader& reader);

void encodeQString(const QString& str, ::cppop::SizedBufferWriter& buf);

}

#endif // SHIM_QSTRING_H
