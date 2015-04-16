// This is a small tool for normalizing signatures generated by Qt's SIGNAL()
// and SLOT() macros.

#include <iostream>
#include <QByteArray>
#include <QMetaObject>
#include <QString>

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage: ./normalize <separated Qt signal signatures to normalize> ...\n";
        return 0;
    }

    for (int i = 1; i < argc; ++i) {
        std::cout << QString(QMetaObject::normalizedSignature(argv[i])).toStdString() << std::endl;
    }

    return 0;
}
