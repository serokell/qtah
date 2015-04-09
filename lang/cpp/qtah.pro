QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtah
TEMPLATE = lib
VERSION = 0.1.0

SOURCES += \
    bindings.cpp \
    listeners.cpp \
    shim_qapplication.cpp

HEADERS += \
    bindings.hpp \
    listeners.hpp \
    shim_qapplication.hpp
