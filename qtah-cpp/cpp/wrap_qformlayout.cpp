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

#include "wrap_qformlayout.hpp"

namespace qtah {
namespace qformlayout {

int getItemRow(const QFormLayout& layout, int index) {
    int row;
    QFormLayout::ItemRole role;
    layout.getItemPosition(index, &row, &role);
    return row;
}

QFormLayout::ItemRole getItemRole(const QFormLayout& layout, int index) {
    int row;
    QFormLayout::ItemRole role;
    layout.getItemPosition(index, &row, &role);
    return role;
}

int getLayoutRow(const QFormLayout& layout, QLayout* layout2) {
    int row;
    QFormLayout::ItemRole role;
    layout.getLayoutPosition(layout2, &row, &role);
    return row;
}

QFormLayout::ItemRole getLayoutRole(const QFormLayout& layout, QLayout* layout2) {
    int row;
    QFormLayout::ItemRole role;
    layout.getLayoutPosition(layout2, &row, &role);
    return role;
}

int getWidgetRow(const QFormLayout& layout, QWidget* widget) {
    int row;
    QFormLayout::ItemRole role;
    layout.getWidgetPosition(widget, &row, &role);
    return row;
}

QFormLayout::ItemRole getWidgetRole(const QFormLayout& layout, QWidget* widget) {
    int row;
    QFormLayout::ItemRole role;
    layout.getWidgetPosition(widget, &row, &role);
    return role;
}

}  // namespace qformlayout
}  // namespace qtah
