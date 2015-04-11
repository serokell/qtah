#include "cbtest.hpp"

#include <iostream>

void testIntCallback(CallbackIntVoid f) {
    std::cout << "Hello from testIntCallback.\n" << std::flush;
    f(42);
}

void testStringCallback(CallbackStringVoid f) {
    std::cout << "Hello from testStringCallback.\n" << std::flush;
    std::string arg("They're on your head.");
    f(arg);
}
