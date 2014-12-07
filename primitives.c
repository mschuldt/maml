#define _PRIMITIVE_

void delay(int seconds);

_PRIMITIVE_
int print_i(int n){
  printf("==> %d\n", n);

_PRIMITIVE_
void test_add(int a, int b){
  print_i(a + b);
}

_PRIMITIVE_
void delay(int ms){
#if arduino
  delay(ms);
#else
  sleep(ms); //TODO: delay for milliseconds instead of seconds
#endif
}

//TODO: way of defining function from standard lib as primitives
_PRIMITIVE_
int quit(int code){
  printf("bye.\n");
  exit(code);
}
