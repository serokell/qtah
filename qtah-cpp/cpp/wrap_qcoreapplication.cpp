// This file is part of Qtah.
//
// Copyright 2015-2018 The Qtah Authors.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "wrap_qcoreapplication.hpp"

#include <cstring>

namespace qtah {
namespace qcoreapplication {

QCoreApplication* create(const QStringList& args) {
    // These leak.  That's okay.  Only one QCoreApplication may be created, and
    // these must be valid for its entire life.
    int* argc = new int(args.size());
    char** argv = new char*[*argc];

    for (int i = 0; i < *argc; ++i) {
        argv[i] = strdup(args[i].toStdString().c_str());
    }

    return new QCoreApplication(*argc, argv);
}

}  // namespace qcoreapplication
}  // namespace qtah
