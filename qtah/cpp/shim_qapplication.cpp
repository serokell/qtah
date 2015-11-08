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

#include <cstring>

namespace qtah {
namespace qapplication {

QApplication* create(const QStringList& args) {
    // These leak.  That's okay.  Only one QApplication may be created, and
    // these must be valid for its entire life.
    int* argc = new int(args.size());
    char** argv = new char*[*argc];

    for (int i = 0; i < *argc; ++i) {
        argv[i] = strdup(args[i].toStdString().c_str());
    }

    return new QApplication(*argc, argv);
}

}
}
