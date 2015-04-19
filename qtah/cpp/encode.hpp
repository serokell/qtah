#include <QMargins>
#include <QRect>

QMargins qMarginsDecode(int* values);

int* qMarginsEncode(const QMargins& margins);

QPoint qPointDecode(int* values);

int* qPointEncode(const QPoint& point);

QRect qRectDecode(int* values);

int* qRectEncode(const QRect& rect);

QSize qSizeDecode(int* values);

int* qSizeEncode(const QSize& size);
