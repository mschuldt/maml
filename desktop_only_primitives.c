_DEFUN_
void print1(struct string *fmt, void *arg){
  char *fmt_str = fmt->s;
  char *c = fmt_str;
  while (*c){
    if (*c == '%' && *(c+1) == 's'){
      printf(fmt_str, ((struct string*)arg)->s);
      return;
    }
    c++;
  }
  printf(fmt_str, arg);
}

_DEFUN_
void delay(int ms){
  sleep(ms); //TODO: delay for milliseconds instead of seconds
}

///primitives used for testing

_DEFUN_
int test_add(int a, int b){
  return a + b;
}
_DEFUN_
void test_add2(int a, int b){
  print_i(a + b);
}
_DEFUN_
void test_add3(int a, int b, int c){
  print_i(a + b + c);
}
_DEFUN_
void test_add4(int a, int b, int c, int d){
  print_i(a + b + c + d);
}
_DEFUN_
void test_add5(int a, int b, int c, int d, int e){
  print_i(a + b + c + d + e);
}
_DEFUN_
void test_add6(int a, int b, int c, int d, int e, int f){
  print_i(a + b + c + d + e + f);
}
_DEFUN_
int test_print_args2(int a, int b){
  printf("args=> %d, %d\n", a, b);
}
_DEFUN_
int test_print_args3(int a, int b, int c){
  printf("args=> %d, %d, %d\n",a,b,c);
}
_DEFUN_
int test_print_args4(int a, int b, int c, int d){
  printf("args=> %d, %d, %d, %d\n",a,b,c,d);
}
_DEFUN_
int test_print_args5(int a, int b, int c, int d, int e){
  printf("args=> %d, %d, %d, %d, %d\n",a,b,c,d,e);
}
_DEFUN_
int test_print_args6(int a, int b, int c, int d, int e, int f){
  printf("args=> %d, %d, %d, %d, %d, %d\n",a,b,c,d,e,f);
}
_DEFUN_
int test_print(int n){
  printf("-> %d\n", n);
  return n;
}
