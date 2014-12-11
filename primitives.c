#define _PRIMITIVE_

void delay(int seconds);

//TODO have the preprocesser extra the number and type of args
//     use that to check for errors while compiling

_PRIMITIVE_
int print_i(int n){
#if arduino
  //TODO:
#else
  printf("%d\n", n);
#endif
}

_PRIMITIVE_
void print_s(struct string* str){
#if arduino
  //TODO:
#else
  printf("%s\n", str->s);
#endif
}

_PRIMITIVE_
void print_a(struct array* a) {
   void** d = a->data;
   for(int i = 0; i < a->len; i++) {
       printf("%d ", (int)d[i]);
   }
   printf("\n");
}

//TODO: way of defining function from standard lib as primitives
_PRIMITIVE_
int die(int code){
#if arduino
  //TODO:
#else
  remove(lockfile);
  printf("bye.\n");
  exit(code);
#endif
}
