#define _PRIMITIVE_

void delay(int seconds);

_PRIMITIVE_
int print_i(int n){
  printf("%d\n", n);
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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///primitives used for testing
#if !arduino
_PRIMITIVE_
void test_add2(int a, int b){
  print_i(a + b);
}
_PRIMITIVE_
void test_add3(int a, int b, int c){
  print_i(a + b + c);
}
_PRIMITIVE_
void test_add4(int a, int b, int c, int d){
  print_i(a + b + c + d);
}
_PRIMITIVE_
void test_add5(int a, int b, int c, int d, int e){
  print_i(a + b + c + d + e);
}
_PRIMITIVE_
void test_add6(int a, int b, int c, int d, int e, int f){
  print_i(a + b + c + d + e + f);
}
#endif
