#ifndef QTAH_SHIM_QLIST_HPP
#define QTAH_SHIM_QLIST_HPP

// This file is part of Qtah.
//
// Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

#include <QList>

namespace qtah {
namespace qlist {

template <typename T>
void put(QList<T>& list, int index, const T& value) {
  list[index] = value;
}

}
}

#endif // QTAH_SHIM_QLIST_HPP
