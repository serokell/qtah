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

#include "encode.hpp"

#include <cstdlib>

QMargins qMarginsDecode(int* values) {
    QMargins result(values[0], values[1], values[2], values[3]);
    free(values);
    return result;
}

int* qMarginsEncode(const QMargins& margins) {
    int* result = static_cast<int*>(malloc(sizeof(int) * 4));
    result[0] = margins.left();
    result[1] = margins.top();
    result[2] = margins.right();
    result[3] = margins.bottom();
    return result;
}

QPoint qPointDecode(int* values) {
    QPoint result(values[0], values[1]);
    free(values);
    return result;
}

int* qPointEncode(const QPoint& point) {
    int* result = static_cast<int*>(malloc(sizeof(int) * 2));
    result[0] = point.x();
    result[1] = point.y();
    return result;
}

QRect qRectDecode(int* values) {
    QRect result(values[0], values[1], values[2], values[3]);
    free(values);
    return result;
}

int* qRectEncode(const QRect& rect) {
    int* result = static_cast<int*>(malloc(sizeof(int) * 4));
    result[0] = rect.x();
    result[1] = rect.y();
    result[2] = rect.width();
    result[3] = rect.height();
    return result;
}

QSize qSizeDecode(int* values) {
    QSize result(values[0], values[1]);
    free(values);
    return result;
}

int* qSizeEncode(const QSize& size) {
    int* result = static_cast<int*>(malloc(sizeof(int) * 2));
    result[0] = size.width();
    result[1] = size.height();
    return result;
}
