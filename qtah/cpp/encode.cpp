#include "encode.hpp"

#include <cstdlib>

QMargins decodeQMargins(int* values) {
    QMargins result(values[0], values[1], values[2], values[3]);
    free(values);
    return result;
}

int* encodeQMargins(const QMargins& margins) {
    int* result = static_cast<int*>(malloc(sizeof(int) * 4));
    result[0] = margins.left();
    result[1] = margins.top();
    result[2] = margins.right();
    result[3] = margins.bottom();
    return result;
}
