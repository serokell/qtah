#ifndef GENERICHANDLER_H
#define GENERICHANDLER_H

#include <QObject>

class GenericHandler : public QObject
{
    Q_OBJECT
public:
    explicit GenericHandler(QObject *parent = 0);

signals:

public slots:
    void fired(QString id);
};

#endif // GENERICHANDLER_H
