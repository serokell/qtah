#ifndef EVENT_H
#define EVENT_H

#include <boost/noncopyable.hpp>
#include <QObject>

namespace qtpi {

class NullarySlot : public QObject, private boost::noncopyable {
    Q_OBJECT

public:

};

}

#endif // EVENT_H
