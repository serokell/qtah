#ifndef QTAH_SHIM_QFORMLAYOUT_HPP
#define QTAH_SHIM_QFORMLAYOUT_HPP

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

}
}

#endif // QTAH_SHIM_QSTRING_HPP
