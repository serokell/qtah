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

#include "wrap_qimage.hpp"

namespace qtah {
namespace qimage {

#if QT_VERSION >= 0x050000

void callCleanup(void* cleanupFunction) {
    CallbackVoid* fn = static_cast<CallbackVoid*>(cleanupFunction);
    (*fn)();
    delete fn;
}

QImage* create(
    uchar* data,
    int width,
    int height,
    QImage::Format format,
    CallbackVoid cleanupFunction) {
    void* cleanupFunctionOnHeap = static_cast<void*>(new CallbackVoid(cleanupFunction));
    return new QImage(data, width, height, format, &callCleanup, cleanupFunctionOnHeap);
}

QImage* create(
    const uchar* data,
    int width,
    int height,
    QImage::Format format,
    CallbackVoid cleanupFunction) {
    void* cleanupFunctionOnHeap = static_cast<void*>(new CallbackVoid(cleanupFunction));
    return new QImage(data, width, height, format, &callCleanup, cleanupFunctionOnHeap);
}

QImage* create(
    uchar* data,
    int width,
    int height,
    int bytesPerLine,
    QImage::Format format,
    CallbackVoid cleanupFunction) {
    void* cleanupFunctionOnHeap = static_cast<void*>(new CallbackVoid(cleanupFunction));
    return new QImage(
        data, width, height, bytesPerLine, format, &callCleanup, cleanupFunctionOnHeap);
}

QImage* create(
    const uchar* data,
    int width,
    int height,
    int bytesPerLine,
    QImage::Format format,
    CallbackVoid cleanupFunction) {
    void* cleanupFunctionOnHeap = static_cast<void*>(new CallbackVoid(cleanupFunction));
    return new QImage(
        data, width, height, bytesPerLine, format, &callCleanup, cleanupFunctionOnHeap);
}

#endif

QImage* create(const QString& fileName, const QString& format) {
    std::string formatStr(format.toStdString());
    return new QImage(fileName, formatStr.c_str());
}

QImage* fromData(const QByteArray& data, const QString& format) {
    std::string formatStr(format.toStdString());
    return new QImage(QImage::fromData(data, formatStr.c_str()));
}

QImage* fromData(const uchar* data, int len, const QString& format) {
    std::string formatStr(format.toStdString());
    return new QImage(QImage::fromData(data, len, formatStr.c_str()));
}

bool load(QImage& image, const QString& fileName, const QString& format) {
    std::string formatStr(format.toStdString());
    return image.load(fileName, formatStr.c_str());
}

bool loadFromData(QImage& image, const QByteArray& data, const QString& format) {
    std::string formatStr(format.toStdString());
    return image.loadFromData(data, formatStr.c_str());
}

bool loadFromData(QImage& image, const uchar* data, int len, const QString& format) {
    std::string formatStr(format.toStdString());
    return image.loadFromData(data, len, formatStr.c_str());
}

bool save(QImage& image, const QString& fileName, const QString& format, int quality) {
    std::string formatStr(format.toStdString());
    return image.save(fileName, formatStr.c_str(), quality);
}

}  // namespace qimage
}  // namespace qtah
