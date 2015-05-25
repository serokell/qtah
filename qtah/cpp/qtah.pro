QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtah
TEMPLATE = lib
VERSION = 0.1.0
# Doesn't seem to work here: CONFIG += c++11
QMAKE_CXXFLAGS += -std=c++11

SOURCES += \
    $$files(b_*.cpp) \
    encode.cpp \
    listener.cpp \
    shim_qapplication.cpp

HEADERS += \
    $$files(b_*.hpp) \
    encode.hpp \
    listener.hpp \
    shim_qapplication.hpp
