#ifndef QTAH_WRAP_QRASTERWINDOW_HPP
#define QTAH_WRAP_QRASTERWINDOW_HPP

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

#if QT_VERSION >= 0x050400

#include <QRasterWindow>
#include <QtGlobal>
#include "b_callback.hpp"

namespace qtah {
namespace qtahrasterwindow {

class QtahRasterWindow : public QRasterWindow {
public:
    QtahRasterWindow(QWindow* parent = Q_NULLPTR) : QRasterWindow(parent) {}
    virtual ~QtahRasterWindow() {}

    virtual void paintEvent(QPaintEvent* paintEvent) {
        if (onPaintEvent_) {
            onPaintEvent_(paintEvent);
        }
    }

    void onPaintEvent(CallbackPtrQPaintEventVoid handler) {
        onPaintEvent_ = handler;
    }

private:
    CallbackPtrQPaintEventVoid onPaintEvent_;
};

}  // namespace qtahrasterwindow
}  // namespace qtah

#endif

#endif // QTAH_WRAP_QRASTERWINDOW_HPP
