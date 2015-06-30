#include "shim_qstring.hpp"

namespace qtah {

void shim_QString_set(QString& str, int position, const QChar& ch) {
    str[position] = ch;
}

}
