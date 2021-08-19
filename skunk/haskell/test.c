#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//void MyModule__onChange__foo(long long* data);
//void MyModule_update(long long* data);

struct MyModule_data_t {
  long foo_value;
  long foo_update;
  long bar_value;
  long bar_update;
  unsigned long bitfield;
};

struct Main_data_t {
  long foo_value;
  long foo_update;
  long bar_value;
  long bar_update;
  long bar_foo_value;
  long bar_foo_update;
  unsigned long bitfield;
  struct MyModule_data_t myModule_1;
  struct MyModule_data_t myModule_2;
};

void Main_update(struct Main_data_t*);

struct Main_data_t* new_data() {
  unsigned size = sizeof(struct Main_data_t);
  struct Main_data_t* data = malloc(size);
  memset(data, 0, size);
  return data;
}

void print(struct Main_data_t* data) {
  printf("------\n");
  printf("foo: %lld (%lld)\n", data->foo_value, data->foo_update);
  printf("bar_foo: %lld (%lld)\n", data->bar_foo_value, data->bar_foo_update);
  printf("bar: %lld (%lld)\n", data->bar_value, data->bar_update);
  printf("[bitfield %lld]\n", data->bitfield);
  printf("MyModule(1):\n");
  printf("\tfoo: %lld (%lld)\n", data->myModule_1.foo_value, data->myModule_1.foo_update);
  printf("\tbar: %lld (%lld)\n", data->myModule_1.bar_value, data->myModule_1.bar_update);
  printf("\t[bitfield %lld]\n", data->myModule_1.bitfield);
  printf("MyModule(2):\n");
  printf("\tfoo: %lld (%lld)\n", data->myModule_2.foo_value, data->myModule_2.foo_update);
  printf("\tbar: %lld (%lld)\n", data->myModule_2.bar_value, data->myModule_2.bar_update);
  printf("\t[bitfield %lld]\n", data->myModule_2.bitfield);
}

int main() {
  struct Main_data_t* data = new_data();
  data->foo_update = 10;
  data->bitfield = 0x01;

  Main_update(data);
  print(data);

  Main_update(data);
  print(data);
  
  Main_update(data);
  print(data);
}
