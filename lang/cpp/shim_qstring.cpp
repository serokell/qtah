#include "shim_qstring.h"

#include <string>

namespace qtpi {

QString decodeQString(::cppop::Server& server, ::cppop::SizedBufferReader& reader) {
    return QString::fromStdString(decodeStdString(server, reader));
}

void encodeQString(const QString& str, ::cppop::SizedBufferWriter& buf) {
    encodeStdString(str.toStdString(), buf);
}

}
