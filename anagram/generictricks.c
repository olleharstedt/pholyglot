#include <limits.h>
#include <stdio.h>
#include <assert.h>

#define G_FORCE(T, V) _Generic((V), T: (V), default: (T){0})

struct A {
  int x;
};

struct B {
  float y;
};

void mycrazyfunction_A_A(int a, int b){ printf("AA %d %d\n",a,b); }
void mycrazyfunction_A_B(int a, float b){ printf("AB %d %f\n",a,b); }
void mycrazyfunction_B_A(float a, int b){ printf("BA %f %d\n",a,b); }

#define mycrazyfunction(X,Y) _Generic((X), \
    struct A: _Generic((Y), \
      struct A: mycrazyfunction_A_A(G_FORCE(struct A, (X)).x, G_FORCE(struct A, (Y)).x), \
      struct B: mycrazyfunction_A_B(G_FORCE(struct A, (X)).x, G_FORCE(struct B, (Y)).y), \
      default: (void)0 \
    ), \
    struct B: _Generic((Y), \
      struct A: mycrazyfunction_B_A(G_FORCE(struct B, (X)).y, G_FORCE(struct A, (Y)).x), \
      default: (void)0 \
    ), \
    default: (void)(struct{ int x; static_assert( \
      _Generic((X), \
        struct A: _Generic((Y), struct A: 1, struct B: 1, default: 0), \
        struct B: _Generic((Y), struct A: 1, default: 0), \
        default: 0 \
      ) \
    , "This type combination isn't supported");}){0} \
  )

int main(){
  struct A a = {42};
  struct B b = {3.14};
  mycrazyfunction(a,a);
  mycrazyfunction(a,b);
  mycrazyfunction(b,a);
  // mycrazyfunction(b,b); // This one would trigger the assert
}
