#ifndef SHIM_QSTRING_H
#define SHIM_QSTRING_H

#include <QString>
#include "buffer.h"

namespace qtpi {

QString decodeQString(::cppop::SizedBufferReader* reader);

void encodeQString(const QString& str, ::cppop::SizedBufferWriter* buf);

}

#endif // SHIM_QSTRING_H
