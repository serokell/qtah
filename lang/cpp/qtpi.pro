#-------------------------------------------------
#
# Project created by QtCreator 2014-12-15T21:06:49
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = qtpi
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    generichandler.cpp \
    qtpi_interface.cpp \
    shim_qapplication.cpp \
    shim_qstring.cpp \
    shim_cbtest.cpp

HEADERS  += mainwindow.h \
    generichandler.h \
    qtpi_interface.h \
    shim_qapplication.h \
    shim_qstring.h \
    event.h \
    shim_cbtest.h

FORMS    += mainwindow.ui

unix|win32: LIBS += -L$$PWD/../../../build-cppop-Desktop-Debug/ -lcppop

INCLUDEPATH += $$PWD/../../../cppop/lang/cpp
DEPENDPATH += $$PWD/../../../cppop/lang/cpp
