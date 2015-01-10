  /*
      The 'Serial' object is BANNED! Use 'maml_serial' instead.
  */

//TODO have the preprocesser extra the number and type of args
//     use that to check for errors while compiling

_DEFUN_(->int)
inline int stack_size(){
  return (stack - stack_base);
}

_DEFUN_(int)
void check_stack(int n){
  //make sure there is at least N free stack slots
  int s = stack_size();
  if (stack_max < s + n){
    stack_max = s + n;
    if (! (stack_base = (void**)realloc(stack_base, sizeof(void*)*stack_max))){
      //TODO: errror
    }
    stack = stack_base + s;
  }
}

_DEFUN_(int, -> int)
int print_i(int n){
#if ARDUINO
  maml_serial.print(n, DEC);
  //delay(1000);
#else
  printf("%d\n", n);
#endif
}

_DEFUN_(str, -> None)
void print_s(struct string* str){
#if ARDUINO
  maml_serial.print(str->s);
  //delay(1000);
#else
  printf("%s\n", str->s);
#endif
}

#if INCLUDE_LISTS //TODO: need to let the preprocessor know not to create
                  //      entries for these functions, it currently
                  //      does not recognize conditional includes
_DEFUN_([int], ->None)
void print_l(struct node *list) {
  struct node *n = list;
  while(n) {
#if ARDUINO
    maml_serial.write((int)n->data);
    maml_serial.write(" ");
#else
    printf("%d ", (int)n->data);
#endif
    n = n->next;
  }
  serial_out("\n");
}
#endif

_DEFUN_((int), ->none)
void print_a(struct array* a) {
  void** d = a->data;
  for(int i = 0; i < a->len; i++) {
#if ARDUINO
    maml_serial.write((int)d[i]);
    maml_serial.write(" ");
#else
    printf("%d ", (int)d[i]);
#endif
  }
  serial_out("\n");
}


//TODO: way of defining function from standard lib as primitives
_DEFUN_(int, -> int)
int die(int code){
#if !ARDUINO
  remove(lockfile);
  printf("bye.\n");
#endif
  exit(code);
}
