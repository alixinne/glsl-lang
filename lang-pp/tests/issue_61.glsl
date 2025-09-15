#version 300 es
#define CS1
#define CS2
#define CS3

vec3 test() {
#if defined(CS1)
  return 1.0;
#elif defined(CS2)
  return 2.0;
#elif defined(CS3)
  return 3.0;
#else
  return 4.0;
#endif
}
