#define _PRIMITIVE_

_PRIMITIVE_
int print_int(int n){
  printf("==> %d\n", n);
  sleep(1);
}

_PRIMITIVE_
void test_add(int a, int b){
  print_int(a + b);
}
