#ifndef QTAH_WRAP_QOPENGLWINDOW_HPP
#define QTAH_WRAP_QOPENGLWINDOW_HPP

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

#include <QOpenGLWindow>
#include <QtGlobal>
#include "b_callback.hpp"

namespace qtah {
namespace qtahopenglwindow {

class QtahOpenGLWindow : public QOpenGLWindow {
public:
    QtahOpenGLWindow(UpdateBehavior updateBehavior = NoPartialUpdate, QWindow* parent = Q_NULLPTR)
        : QOpenGLWindow(updateBehavior, parent) {}

    QtahOpenGLWindow(
        QOpenGLContext* shareContext,
        UpdateBehavior updateBehavior = NoPartialUpdate,
        QWindow *parent = Q_NULLPTR)
        : QOpenGLWindow(shareContext, updateBehavior, parent) {}

    virtual ~QtahOpenGLWindow() {}

    virtual void onInitializeGL() {
        if (onInitializeGL_) {
            onInitializeGL_();
        }
    }

    virtual void onPaintGL() {
        if (onPaintGL_) {
            onPaintGL_();
        }
    }

    virtual void onPaintOverGL() {
        if (onPaintOverGL_) {
            onPaintOverGL_();
        }
    }

    virtual void onPaintUnderGL() {
        if (onPaintUnderGL_) {
            onPaintUnderGL_();
        }
    }

    virtual void onResizeGL(int width, int height) {
        if (onResizeGL_) {
            onResizeGL_(width, height);
        }
    }

    void onInitializeGL(CallbackVoid handler) {
        onInitializeGL_ = handler;
    }

    void onPaintGL(CallbackVoid handler) {
        onPaintGL_ = handler;
    }

    void onPaintOverGL(CallbackVoid handler) {
        onPaintOverGL_ = handler;
    }

    void onPaintUnderGL(CallbackVoid handler) {
        onPaintUnderGL_ = handler;
    }

    void onResizeGL(CallbackIntIntVoid handler) {
        onResizeGL_ = handler;
    }

private:
    CallbackVoid onInitializeGL_;
    CallbackVoid onPaintGL_;
    CallbackVoid onPaintOverGL_;
    CallbackVoid onPaintUnderGL_;
    CallbackIntIntVoid onResizeGL_;
};

}  // namespace qtahopenglwindow
}  // namespace qtah

#endif

#endif // QTAH_WRAP_QOPENGLWINDOW_HPP
