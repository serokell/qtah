#include "shim_qstring.h"
#include <QString>
#include <string>
#include "buffers.h"

namespace qtpi {

QString decodeQString(::cppop::BufferReader* reader) {
    return QString::fromStdString(decodeStdString(reader));
}

void encodeQString(const QString& str, ::cppop::WritableBuffer* buf) {
    encodeStdString(str.toStdString(), buf);
}

}
