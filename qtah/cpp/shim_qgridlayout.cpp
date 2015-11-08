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

#include "shim_qgridlayout.hpp"

namespace qtah {
namespace qgridlayout {

#if QT_VERSION >= 0x050000
int getItemRow(const QGridLayout& layout, int index) {
#else
int getItemRow(QGridLayout& layout, int index) {
#endif
    int row, column, rowSpan, columnSpan;
    layout.getItemPosition(index, &row, &column, &rowSpan, &columnSpan);
    return row;
}

#if QT_VERSION >= 0x050000
int getItemColumn(const QGridLayout& layout, int index) {
#else
int getItemColumn(QGridLayout& layout, int index) {
#endif
    int row, column, rowSpan, columnSpan;
    layout.getItemPosition(index, &row, &column, &rowSpan, &columnSpan);
    return column;
}

#if QT_VERSION >= 0x050000
int getItemRowSpan(const QGridLayout& layout, int index) {
#else
int getItemRowSpan(QGridLayout& layout, int index) {
#endif
    int row, column, rowSpan, columnSpan;
    layout.getItemPosition(index, &row, &column, &rowSpan, &columnSpan);
    return rowSpan;
}

#if QT_VERSION >= 0x050000
int getItemColumnSpan(const QGridLayout& layout, int index) {
#else
int getItemColumnSpan(QGridLayout& layout, int index) {
#endif
    int row, column, rowSpan, columnSpan;
    layout.getItemPosition(index, &row, &column, &rowSpan, &columnSpan);
    return columnSpan;
}

}
}
