#ifndef SHIM_QAPPLICATION_H
#define SHIM_QAPPLICATION_H

#include <QApplication>

namespace qtah {

void shiminit_QApplication(int argc, char *argv[]);

QApplication* shim_QApplication_new();

}

#endif // SHIM_QAPPLICATION_H
