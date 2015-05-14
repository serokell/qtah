QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtah
TEMPLATE = lib
VERSION = 0.1.0
# Doesn't seem to work here: CONFIG += c++11
QMAKE_CXXFLAGS += -std=c++11

SOURCES += \
    callback.cpp \
    core.cpp \
    encode.cpp \
    listener.cpp \
    listener-bindings.cpp \
    shim_qapplication.cpp \
    std.cpp \
    widgets.cpp

HEADERS += \
    callback.hpp \
    core.hpp \
    encode.hpp \
    listener.hpp \
    listener-bindings.hpp \
    shim_qapplication.hpp \
    std.hpp \
    widgets.hpp
