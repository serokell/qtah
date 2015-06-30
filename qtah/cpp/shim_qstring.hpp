#ifndef QTAH_SHIM_QSTRING_HPP
#define QTAH_SHIM_QSTRING_HPP

#include <QString>

namespace qtah {

void shim_QString_set(QString& str, int position, const QChar& ch);

}

#endif // QTAH_SHIM_QSTRING_HPP
