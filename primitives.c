#define _PRIMITIVE_                             \
  /*
      The 'Serial' object is BANNED! Use 'maml_serial' instead.
  */
//TODO have the preprocesser extra the number and type of args
//     use that to check for errors while compiling

_PRIMITIVE_
int print_i(int n){
#if arduino
  maml_serial.print(n, DEC);
  delay(1000);
#else
  printf("%d\n", n);
#endif
}

_PRIMITIVE_
void print_s(struct string* str){
#if arduino
  maml_serial.print(str->s);
  delay(1000);
#else
  printf("%s\n", str->s);
#endif
}

_PRIMITIVE_
void print_l(struct node *list) {
  struct node *n = list;
  while(n) {
#if arduino
    maml_serial.write((int)n->data);
    maml_serial.write(" ");
#else
    printf("%d ", (int)n->data);
#endif
    n = n->next;
  }
  serial_out("\n");
}

_PRIMITIVE_
void print_a(struct array* a) {
  void** d = a->data;
  for(int i = 0; i < a->len; i++) {
#if arduino
    maml_serial.write((int)d[i]);
    maml_serial.write(" ");
#else
    printf("%d ", (int)d[i]);
#endif
  }
  serial_out("\n");
}


//TODO: way of defining function from standard lib as primitives
_PRIMITIVE_
int die(int code){
#if !arduino
  remove(lockfile);
  printf("bye.\n");
#endif
  exit(code);
}


#if !arduino
#include <time.h>
double cps_div_1000 = CLOCKS_PER_SEC / 1000.0;
#endif

_PRIMITIVE_
 long milliseconds(void){
#if arduino
   return millis();
#else
   return (clock() / cps_div_1000);
#endif
 }
