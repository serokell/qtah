#ifndef QTAH_WRAP_QMETAOBJECT_HPP
#define QTAH_WRAP_QMETAOBJECT_HPP

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

#include <QMetaObject>
#include <QString>

namespace qtah {
namespace qmetaobject {

int indexOfClassInfo(const QMetaObject& self, const QString& name);

int indexOfConstructor(const QMetaObject& self, const QString& name);

int indexOfEnumerator(const QMetaObject& self, const QString& name);

int indexOfMethod(const QMetaObject& self, const QString& name);

int indexOfProperty(const QMetaObject& self, const QString& name);

int indexOfSignal(const QMetaObject& self, const QString& name);

int indexOfSlot(const QMetaObject& self, const QString& name);

}  // namespace qmetaobject
}  // namespace qtah

#endif // QTAH_WRAP_QMETAOBJECT_HPP
