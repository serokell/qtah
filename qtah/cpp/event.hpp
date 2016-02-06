#ifndef QTAH_EVENT_HPP
#define QTAH_EVENT_HPP

// This file is part of Qtah.
//
// Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

#include <QEvent>
#include "b_callback.hpp"

namespace qtah {
namespace event {

class EventListener : public QObject {
    Q_OBJECT

public:
    EventListener(CallbackPtrQObjectPtrQEventBool callback, int* deletedPtr) :
        callback_(callback), deleted_(deletedPtr) {}

    ~EventListener() {
        if (deleted_) {
            *deleted_ = 1;
        }
    }

    virtual bool eventFilter(QObject* receiver, QEvent* event) {
        return callback_(receiver, event);
    }

private:
    CallbackPtrQObjectPtrQEventBool callback_;
    int* deleted_;
};

}  // namespace event
}  // namespace qtah

#endif // QTAH_EVENT_HPP
