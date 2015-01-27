#ifndef SHIM_QSTRING_H
#define SHIM_QSTRING_H

#include <QString>
#include "buffers.h"

namespace qtpi {

QString decodeQString(::cppop::BufferReader* reader);

void encodeQString(const QString& str, ::cppop::WritableBuffer* buf);

}

#endif // SHIM_QSTRING_H
