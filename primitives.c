  /*
      The 'Serial' object is BANNED! Use 'maml_serial' instead.
  */

//TODO have the preprocesser extra the number and type of args
//     use that to check for errors while compiling


_DEFUN_
int print_i(int n){
#if arduino
  maml_serial.print(n, DEC);
  delay(1000);
#else
  printf("%d\n", n);
#endif
}

_DEFUN_
void print_s(struct string* str){
#if arduino
  maml_serial.print(str->s);
  delay(1000);
#else
  printf("%s\n", str->s);
#endif
}

_DEFUN_
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

_DEFUN_
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
_DEFUN_
int die(int code){
#if !arduino
  remove(lockfile);
  printf("bye.\n");
#endif
  exit(code);
}
