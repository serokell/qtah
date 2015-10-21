// This file is part of Qtah.
//
// Copyright 2015 Bryan Gardiner <bog@khumba.net>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License version 3
// as published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "shim_qapplication.hpp"

#include <QApplication>

namespace {

static int shim_argc;
static char** shim_argv;

}

namespace qtah {

void shiminit_QApplication(int argc, char **argv) {
    shim_argc = argc;
    shim_argv = argv;
}

QApplication* shim_QApplication_new() {
    return new QApplication(shim_argc, shim_argv);
}

}
