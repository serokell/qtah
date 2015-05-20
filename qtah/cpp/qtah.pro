QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtah
TEMPLATE = lib
VERSION = 0.1.0
# Doesn't seem to work here: CONFIG += c++11
QMAKE_CXXFLAGS += -std=c++11

SOURCES += \
    callback.cpp \
    encode.cpp \
    gen_qcoreapplication.cpp \
    gen_qmargins.cpp \
    gen_qobject.cpp \
    gen_qpoint.cpp \
    gen_qrect.cpp \
    gen_qsize.cpp \
    gen_qstring.cpp \
    listener.cpp \
    listener-bindings.cpp \
    shim_qapplication.cpp \
    std.cpp \
    types.cpp \
    widgets.cpp

HEADERS += \
    callback.hpp \
    encode.hpp \
    gen_qcoreapplication.hpp \
    gen_qmargins.hpp \
    gen_qobject.hpp \
    gen_qpoint.hpp \
    gen_qrect.hpp \
    gen_qsize.hpp \
    gen_qstring.hpp \
    listener.hpp \
    listener-bindings.hpp \
    shim_qapplication.hpp \
    std.hpp \
    types.hpp \
    widgets.hpp
