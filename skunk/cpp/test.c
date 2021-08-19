#include <stdio.h>

void MyModule__onChange__foo(long long* data);
void MyModule_update(long long* data);

int main() {
  long long data[5];
  for (unsigned i = 0; i < 5; i++)
    data[i] = 0LL;
  
  data[1] = 10LL;
  data[4] = 1LL;
  printf("%lld\n", data[0]);
  printf("%lld\n", data[1]);
  printf("%lld\n", data[2]);
  printf("%lld\n", data[3]);
  printf("%lld\n", data[4]);
  printf("---\n");
  // MyModule__onChange__foo(data);
  MyModule_update(data);
  printf("%lld\n", data[0]);
  printf("%lld\n", data[1]);
  printf("%lld\n", data[2]);
  printf("%lld\n", data[3]);
  printf("%lld\n", data[4]);
}