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

#include "wrap_qobject.hpp"

namespace qtah {
namespace qobject {

bool inherits(const QObject& obj, const QString& className) {
    return obj.inherits(className.toStdString().c_str());
}

QVariant property(const QObject& obj, const QString& propertyName) {
    return obj.property(propertyName.toStdString().c_str());
}

void setProperty(QObject& obj, const QString& propertyName, const QVariant& value) {
    obj.setProperty(propertyName.toStdString().c_str(), value);
}

}  // namespace qobject
}  // namespace qtah
