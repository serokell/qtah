QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtpi
TEMPLATE = lib
VERSION = 0.1.0

SOURCES += \
    bindings.cpp \
    shim_qapplication.cpp

HEADERS += \
    bindings.hpp \
    shim_qapplication.h
