#ifndef QTAH_SHIM_QAPPLICATION_HPP
#define QTAH_SHIM_QAPPLICATION_HPP

#include <QApplication>

namespace qtah {

void shiminit_QApplication(int argc, char *argv[]);

QApplication* shim_QApplication_new();

}

#endif // QTAH_SHIM_QAPPLICATION_HPP
