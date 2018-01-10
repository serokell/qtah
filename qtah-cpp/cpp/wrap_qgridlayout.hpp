#ifndef QTAH_WRAP_QGRIDLAYOUT_HPP
#define QTAH_WRAP_QGRIDLAYOUT_HPP

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

#include <QGridLayout>
#include <QLayout>
#include <QWidget>

namespace qtah {
namespace qgridlayout {

#if QT_VERSION >= 0x050000
int getItemRow(const QGridLayout& layout, int index);
int getItemColumn(const QGridLayout& layout, int index);
int getItemRowSpan(const QGridLayout& layout, int index);
int getItemColumnSpan(const QGridLayout& layout, int index);
#else
int getItemRow(QGridLayout& layout, int index);
int getItemColumn(QGridLayout& layout, int index);
int getItemRowSpan(QGridLayout& layout, int index);
int getItemColumnSpan(QGridLayout& layout, int index);
#endif

}  // namespace qgridlayout
}  // namespace qtah

#endif // QTAH_WRAP_QGRIDLAYOUT_HPP
