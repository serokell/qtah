#include "generichandler.h"
#include <iostream>

GenericHandler::GenericHandler(QObject *parent) :
    QObject(parent)
{
}

void GenericHandler::fired(QString id)
{
    std::cout << "Event received from " << id.toStdString() << ".\n" << std::flush;
}
