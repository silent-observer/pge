#include <immintrin.h>
#include <math.h>

#define DEFINE_FUNC(func) __m256d func##Avx(__m256d x) { \
    double a[4]; \
    _mm256_storeu_pd(a, x); \
    a[0] = func(a[0]); \
    a[1] = func(a[1]); \
    a[2] = func(a[2]); \
    a[3] = func(a[3]); \
    return _mm256_set_pd(a[3], a[2], a[1], a[0]); \
}

DEFINE_FUNC(fabs)
DEFINE_FUNC(exp)
DEFINE_FUNC(log)
DEFINE_FUNC(sqrt)
DEFINE_FUNC(sin)
DEFINE_FUNC(cos)
DEFINE_FUNC(asin)
DEFINE_FUNC(acos)
DEFINE_FUNC(erf)

__m256d atan2Avx(__m256d y, __m256d x) {
    double a[4];
    double b[4];
    _mm256_storeu_pd(a, y);
    _mm256_storeu_pd(b, x);
    a[0] = atan2(a[0], b[0]);
    a[1] = atan2(a[1], b[1]);
    a[2] = atan2(a[2], b[2]);
    a[3] = atan2(a[3], b[3]);
    return _mm256_set_pd(a[3], a[2], a[1], a[0]);
}