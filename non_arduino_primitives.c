///primitives used for testing
_PRIMITIVE_
int test_add(int a, int b){
  return a + b;
}
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
_PRIMITIVE_
int test_print_args2(int a, int b){
  printf("args=> %d, %d\n", a, b);
}
_PRIMITIVE_
int test_print_args3(int a, int b, int c){
  printf("args=> %d, %d, %d\n",a,b,c);
}
_PRIMITIVE_
int test_print_args4(int a, int b, int c, int d){
  printf("args=> %d, %d, %d, %d\n",a,b,c,d);
}
_PRIMITIVE_
int test_print_args5(int a, int b, int c, int d, int e){
  printf("args=> %d, %d, %d, %d, %d\n",a,b,c,d,e);
}
_PRIMITIVE_
int test_print_args6(int a, int b, int c, int d, int e, int f){
  printf("args=> %d, %d, %d, %d, %d, %d\n",a,b,c,d,e,f);
}
_PRIMITIVE_
int test_print(int n){
  printf("-> %d\n", n);
  return n;
}