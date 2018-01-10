#ifndef QTAH_WRAP_QFORMLAYOUT_HPP
#define QTAH_WRAP_QFORMLAYOUT_HPP

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

#include <QFormLayout>
#include <QLayout>
#include <QWidget>

namespace qtah {
namespace qformlayout {

int getItemRow(const QFormLayout& layout, int index);

QFormLayout::ItemRole getItemRole(const QFormLayout& layout, int index);

int getLayoutRow(const QFormLayout& layout, QLayout* layout2);

QFormLayout::ItemRole getLayoutRole(const QFormLayout& layout, QLayout* layout2);

int getWidgetRow(const QFormLayout& layout, QWidget* widget);

QFormLayout::ItemRole getWidgetRole(const QFormLayout& layout, QWidget* widget);

}  // namespace qformlayout
}  // namespace qtah

#endif // QTAH_WRAP_QFORMLAYOUT_HPP
