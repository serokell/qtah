#include "mainwindow.h"
#include "generichandler.h"
#include <iostream>
#include <QApplication>
#include <QWidget>
#include <QPushButton>
#include <QLabel>
#include <QHBoxLayout>
#include <QSignalMapper>
#include "driver.h"
#include "qtpi_interface.h"
#include "shim_qapplication.h"

static cppop::InterfaceFn interfaces[] = {
    &geniface__qtpi,
};

int main(int argc, char* argv[]) {
    std::cerr << "Qtpi starting up...\n";
    qtpi::shiminit_QApplication(argc, argv);
    return cppop::executeWithArgs(
        argc,
        argv,
        sizeof(interfaces) / sizeof(interfaces[0]),
        interfaces);
}

int foo(int argc, char *argv[]) {
    QApplication a(argc, argv);
    //MainWindow w;
    //w.dumpObjectInfo();
    //w.dumpObjectTree();
    //w.show();

    QWidget w;
    w.resize(640, 480);
    w.setWindowTitle("Hi thar");
    QHBoxLayout hb(&w);
    QLabel l;
    l.setText("0");
    QPushButton b;
    b.setText("Increment");
    hb.addWidget(&l);
    hb.addWidget(&b);

    QSignalMapper sm(&w);
    GenericHandler gh;
    sm.setMapping(&b, QString("<inc>"));
    QObject::connect(&b, SIGNAL(clicked()), &sm, SLOT(map()));
    QObject::connect(&sm, SIGNAL(mapped(QString)), &gh, SLOT(fired(QString)));

    w.show();
    std::cout << "Running!\n" << std::flush;
    return a.exec();
}
