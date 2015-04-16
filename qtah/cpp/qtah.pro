QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtah
TEMPLATE = lib
VERSION = 0.1.0
# Doesn't seem to work here: CONFIG += c++11
QMAKE_CXXFLAGS += -std=c++11

SOURCES += \
    bindings.cpp \
    callbacks.cpp \
    listeners.cpp \
    shim_qapplication.cpp

HEADERS += \
    bindings.hpp \
    callbacks.hpp \
    listeners.hpp \
    shim_qapplication.hpp
