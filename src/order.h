#ifndef ORDER_H
#define ORDER_H

#include <limits.h>
#include <stdint.h>

#if CHAR_BIT != 8
#error "Unsupported char size."
#endif

enum
{
  LITTLE_ENDIAN = 0x03020100ul,
  BIG_ENDIAN = 0x00010203ul
};

static const union { unsigned char bytes[4]; uint32_t value; } host_order =
  { { 0, 1, 2, 3 } };

#define HOST_ORDER (host_order.value)

#endif

