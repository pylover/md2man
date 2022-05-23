# STRDUP

## Name

strdup, strndup, strdupa, strndupa - duplicate a string

## Synopsis

```C
#include <string.h>

char *strdup(const char *s);

char *strndup(const char *s, size_t n);
char *strdupa(const char *s);
char *strndupa(const char *s, size_t n);

```

Feature Test Macro Requirements for glibc (see feature_test_macros(7)):

```C
strdup():
    _XOPEN_SOURCE >= 500
        || /* Since glibc 2.12: */ _POSIX_C_SOURCE >= 200809L
        || /* Glibc versions <= 2.19: */ _BSD_SOURCE || _SVID_SOURCE
strndup():
    Since glibc 2.10:
        _POSIX_C_SOURCE >= 200809L
    Before glibc 2.10:
        _GNU_SOURCE
strdupa(), strndupa(): _GNU_SOURCE
```
