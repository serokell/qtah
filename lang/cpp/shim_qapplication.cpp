#include "shim_qapplication.h"

#include <QApplication>

namespace {

static int shim_argc;
static char** shim_argv;

}

namespace qtpi {

void shiminit_QApplication(int argc, char **argv) {
    shim_argc = argc;
    shim_argv = argv;
}

QApplication* shim_QApplication_new() {
    return new QApplication(shim_argc, shim_argv);
}

}
