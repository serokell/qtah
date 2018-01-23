#ifndef QTAH_WRAP_QIMAGE_HPP
#define QTAH_WRAP_QIMAGE_HPP

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

#include <QByteArray>
#include <QImage>
#include <QtGlobal>
#include "b_callback.hpp"

namespace qtah {
namespace qimage {

#if QT_VERSION >= 0x050000
QImage* create(uchar*, int, int, QImage::Format, CallbackVoid);
QImage* create(const uchar*, int, int, QImage::Format, CallbackVoid);
QImage* create(uchar*, int, int, int, QImage::Format, CallbackVoid);
QImage* create(const uchar*, int, int, int, QImage::Format, CallbackVoid);
#endif
QImage* create(const QString&, const QString&);

QImage* fromData(const uchar*, int, const QString&);

QImage* fromData(const QByteArray&, const QString&);

bool load(QImage&, const QString&, const QString&);

bool loadFromData(QImage&, const QByteArray&, const QString&);

bool loadFromData(QImage&, const uchar*, int, const QString&);

bool save(QImage&, const QString&, const QString&, int);

}  // namespace qimage
}  // namespace qtah

#endif // QTAH_WRAP_QIMAGE_HPP
