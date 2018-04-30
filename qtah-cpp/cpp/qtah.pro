# This file is part of Qtah.
#
# Copyright 2015-2018 The Qtah Authors.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtah
TEMPLATE = lib
VERSION = 0.4.0
# Doesn't seem to work here: CONFIG += c++11
QMAKE_CXXFLAGS += -std=c++11

mac:QMAKE_SONAME_PREFIX = @rpath

SOURCES += \
    $$files(b_*.cpp) \
    $$files(wrap_*.cpp) \
    encode.cpp \
    listener.cpp

HEADERS += \
    $$files(b_*.hpp) \
    $$files(wrap_*.hpp) \
    encode.hpp \
    event.hpp \
    listener.hpp

target.path = /
INSTALLS += target
