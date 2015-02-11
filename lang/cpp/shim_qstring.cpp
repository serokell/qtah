#include "shim_qstring.h"

#include <string>

namespace qtpi {

QString decodeQString(::cppop::SizedBufferReader* reader) {
    return QString::fromStdString(decodeStdString(reader));
}

void encodeQString(const QString& str, ::cppop::SizedBufferWriter* buf) {
    encodeStdString(str.toStdString(), buf);
}

}
