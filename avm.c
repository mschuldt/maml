// AVM - Arduino Virtual Machine

//TODO: threaded code should be optional

#define arduino 0
#define SERIAL_INTR_PIN 0
#define DEBUG 0

#define include_lists 1

//NOTE: If these are changed, their value in maml_serial.py must be also changed.
#define BYTECODE_IN_FILE "_bc.txt"
#define NUM_TERMINATOR 'x'

////////////////////////////////////////////////////////////////////////////////

#include "_opcodes.h"

#if ! arduino
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#endif

#define NEXT(code) goto *(*code++)

#if DEBUG
#define D(...) printf(__VA_ARGS__);
#else
#define D(...)
#endif

////////////////////////////////////////////////////////////////////////////////

#ifndef true
#define true 1
#endif
#ifndef false
#define false 0
#endif
#ifndef NULL
#define NULL 0
#endif

#if arduino
#define DIE //?
#define SAY(words) Serial.write(words)
#else
#define DIE(n) exit(n)
#define SAY(words) printf(words)
#endif

#define _PRIMITIVE_

struct string{
  int len;
  char* s;
};

struct array{
  int len;
  void* data;
};

struct procedure{
  int n_args;
  void** code;
  int n_locals;
  int* args;
};

void init_procedure(struct procedure* fn, int code_len, int n_locals){
  fn->n_args = 0;
  fn->n_locals = n_locals;
  //+ 1 for the END_OF_BLOCK instruction
  fn->code = (void**)malloc(sizeof(void*)*(code_len + 1));
}

struct frame{
  void** code;
  void** locals;
  int n_locals;
  struct frame* prev;
  struct frame* next;
};

struct codeblock{
  int index; //index in codeblock chain
  void** code;
  int len; //length of 'code'
  struct codeblock* next;
  struct codeblock* prev;
};

struct codeblock* blockchain;
struct codeblock* blockchain_end;
int n_codeblocks;

void init_codeblock(struct codeblock* block, int code_len){
  block->index = -1;
  block->len = code_len;
  //+ 1 for the END_OF_BLOCK instruction
  block->code = (void**)malloc(sizeof(void*)*(code_len + 1));
  block->prev = block->next = NULL;
}

void append_codeblock(struct codeblock* block){
  //TODO: should add
  if (!blockchain_end){
    block->index = 0;
    blockchain = blockchain_end = block;
    block->next = block;
  }else{
    blockchain_end->next = block;
    block->next = blockchain;
    block->prev = blockchain_end;
    block->index = blockchain_end->index + 1;
    blockchain_end = block;
  }
  n_codeblocks++;
}

#if include_lists
struct node{
  void* data;
  struct node* next;
};

_PRIMITIVE_
struct node* cons(void* d, struct node* list){
  struct node* _new = (struct node*)malloc(sizeof(struct node));
  _new->data = d;
  _new->next = list;
  return _new;
}
_PRIMITIVE_
void* car(struct node* list){
  return list->data;
}
_PRIMITIVE_
struct node* cdr(struct node* list){
  return list->next;
}
#endif

void serial_in();

// 'setup' and 'loop' are used so that this code will
// work with the standard Arduino IDE as well

void loop();

#if !arduino
char* lockfile;
#endif

void** globals; //array of global variables
int max_globals = 20; //TODO: maml.env.py needs to know about this
                      //      OR: have serial_in() check and resize!
struct frame *current_frame = NULL;

//if these names are changed, also change them in process_primitives.el
void** primitives; //this is filled by auto-generated code in _prim.c
int n_primitives;
#include "primitives.c"
#if ! arduino
#include "non_arduino_primitives.c"
#endif

void setup(void){
#include "_prim.c"

  blockchain = NULL;
  blockchain_end = NULL;
  n_codeblocks = 0;
  globals = malloc(sizeof(void*)*max_globals);

#if arduino // setup serial
  pinMode(2, INPUT);
  digitalWrite(2, LOW);
  Serial.begin(9600);
  attachInterrupt(SERIAL_INTR_PIN, serial_in, CHANGE);
#else // setup signal interrupt
  printf("Initializing avm...\n");
  lockfile = (char*)malloc(sizeof(char)*15);
  sprintf(lockfile, "%d.lock", getpid());
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "0");
  fclose(fp);

  signal(SIGIO, (__sighandler_t)serial_in);
  //TODO: catch kill signal and remove lock file
#endif

  loop();// variables are initialized the first time loop is called

#if arduino
  //TODO
#else
  printf("Ready.\n\n");
#endif
}

void* labels[20];
char initialized = false;
void* l_load_const;
void* l_addii;
void* l_addi;
void* l_add;
void* l_ret;
void* l_end_of_block;
void* l_call_prim_0;
void* l_call_prim_1;
void* l_call_prim_2;
void* l_call_prim_3;
void* l_call_prim_4;
void* l_call_prim_5;
void* l_call_prim_6;
void* l_pop;
void* l_store_global;
void* l_load_global;
void* l_jump;
void* l_if;
void* l_lt;
void* l_sub;
#if include_lists
void* l_list;
#endif
static int int_regs[8];
static char* char_regs[8];

void loop (){
  if (!initialized){
    initialized = true;
    l_load_const = &&load_const;
    l_add = &&add;
    l_ret = &&ret;
    l_end_of_block = &&end_of_block;
    l_call_prim_0 = &&call_prim_0;
    l_call_prim_1 = &&call_prim_1;
    l_call_prim_2 = &&call_prim_2;
    l_call_prim_3 = &&call_prim_3;
    l_call_prim_4 = &&call_prim_4;
    l_call_prim_5 = &&call_prim_5;
    l_call_prim_6 = &&call_prim_6;
    l_pop = &&pop;
    l_load_global = &&load_global;
    l_store_global = &&store_global;
    l_jump = &&jump;
    l_if = &&_if;
    l_lt = &&lt;
    l_sub = &&sub;
#if include_lists
    l_list = &&list;
#endif
    return;
  }
  if (!blockchain) return;//no bytecode yet

  //this part happens only once
  struct codeblock* current_block = blockchain;
  int i_top=0, c_top=0;
  void** code = current_block->code;
  int *I = &int_regs[0];
  char **C = &char_regs[0];
  static long a,b,c,d,e,f,g;

  void* stack[10];//??
  int top = -1;//index of the top item on the stack

  void* r_ret; //?
  int n;
  struct node *list = NULL;
  struct node *tmp;

  NEXT(code);

 load_const:
  D("loading const: %d\n", *code);
  stack[++top] = *code;
  code++;
  NEXT(code);
 call_prim_0:
  D("call_0\n");
  stack[++top] = ((void* (*)(void))(*code))();
  NEXT(code);
 call_prim_1:
  D("call_1\n");
  stack[top] = ((void* (*)(void*))(*code++))(stack[top]);
  NEXT(code);
 call_prim_2:
  //// define _ and S////////////////////////////////////////////
#define _ void*
#define S(i) stack[top+i]
  D("call_2\n");
  stack[--top] = ((_ (*)(_, _))(*code++))(S(0), S(1));
  NEXT(code);
 call_prim_3:
  D("call_3\n");
  top -= 2;
  stack[top] = ((_ (*)(_, _, _))(*code++))(S(0),S(1), S(2));
  NEXT(code);
 call_prim_4:
  D("call_4\n");
  top -= 3;
  stack[top] = ((_ (*)(_, _, _, _))(*code++))(S(0),S(1),S(2),S(3));
  NEXT(code);
 call_prim_5:
  D("call_5\n");
  top -= 4;
  stack[top] = ((_ (*)(_, _, _, _, _))(*code++))(S(0),S(1),S(2),S(3),S(4));
  NEXT(code);
 call_prim_6:
  D("call_6\n");
  top -= 5;
  stack[top] = ((_ (*)(_, _, _, _, _, _))(*code++))(S(0),S(1),S(2),S(3),S(4),S(5));
  NEXT(code);
#undef _
#undef S
  //// undef _ and S ////////////////////////////////////////////
 call:
  NEXT(code);
 ret:
  NEXT(code);
 load_global:
  D("load_global\n")
    stack[++top] = globals[(int)*code++];
  NEXT(code);
 store_global:
  D("store_global\n")
    globals[(int)*code++] = stack[top--];
  NEXT(code);
 _if:
  D("if\n");
  if (stack[top--]){
    code+=2; //skip over the jump
  }
  NEXT(code);
 jump:
  code = (void**)*code;
  NEXT(code);
 add:
  D("add\n");
  stack[top-1] = (void*)((int)stack[top] + (int)stack[--top]);
  NEXT(code);
 sub:
  D("add\n");
  stack[top-1] = (void*)((int)stack[top-1] - (int)stack[top--]);
  NEXT(code);
 lt:
  // use > because items on stack are reversed
  stack[top-1] = (void*) ((int)(stack[top]) > ((int)stack[--top]));
  NEXT(code);
#if include_lists
 list:
  D("list\n");
  n = (int)*code++;
  list = NULL;
  while (n){
    tmp = (struct node*)malloc(sizeof(struct node));
    tmp->next = list;
    tmp->data = stack[top--];
    list = tmp;
    n--;
  }
  stack[++top] = list;
  NEXT(code);
#endif
 end_of_block:
  current_block = current_block->next;
  code = current_block->code;
  NEXT(code);
 pop:
  D("POP\n");
  --top;
  NEXT(code);
}

#if arduino
#else
int main(){
  setup();
  //serial_in(); ///test
  while (1){
    loop();
  }
  return 0;
}
#endif

#if arduino
volatile boolean receiving_serial = false;
#endif

/* when this runs on the Arduino, 'serial_in' will be called
   multiple times while a single string of bytecodes is being
   transferred. When it runs on Linux, it will only be called
   once to read bytecodes in from a file. Bytecodes are communicated
   using a persistent file for debugging purposes.
*/
#if arduino
#define SKIP(ch, msg) if (Serial.read() != ch){         \
    Serial.print("Error -- skip: unexpected char");     \
  }

#else
#define SKIP(ch, msg) if (fgetc(fp) != ch){             \
    printf("Error: expected char '%c'"msg"\n", ch);     \
    exit(1);                                            \
  }
#endif

//TODO: equivalent functions for Arduino
#if arduino
int read_int(){
  char integer[10];
  int i=0;
  char ch;
  while ((ch = Serial.read()) != NUM_TERMINATOR){
    integer[i++] = ch;
  }
  integer[i] = '\0';
  return atoi(integer);
}
#else
int read_int(FILE* fp){
  char integer[10];
  int i=0;
  char ch;
  while ((ch = fgetc(fp)) != NUM_TERMINATOR){
    integer[i++] = ch;
    SKIP('\n', "(read_int)");
  }
  SKIP('\n', "(read_int)");
  integer[i] = '\0';
  return atoi(integer);
}
#endif

#if arduino
struct string* read_string(){
  //TODO
}
#else
struct string* read_string(FILE* fp){
  int n = read_int(fp);
  //TODO: check that we have enough mem
  char* s = (char*)malloc(sizeof(char)*n+1);
  struct string *str = (struct string*)malloc(sizeof(struct string));
  str->s = s;
  str->len = n;
  int i = 0;
  while (*s++ = fgetc(fp)){
    if (++i > n){
      SAY("Error: max string size exceeded\n"); DIE(1);
    }
    SKIP('\n', "(read_int)");
  }
  SKIP('\n', "(read_int)");
  *s = '\0';
  return str;
}
#endif
#if arduino
int* read_int_array(FILE* fp){
  //TODO
}
#else
int* read_int_array(FILE* fp){
  int n = read_int(fp);
  //TODO: check that we have enough mem
  int* a = (int*)malloc(sizeof(int)*n);
  int i = 0;
  while (*a++ = fgetc(fp)){
    if (++i > n){
      SAY("Error: max int array size exceeded"); DIE(1);
    }
    SKIP('\n', "(read_int_array)");
  }
  SKIP('\n', "(read_int_array)");
  return a;
}
#endif

//TODO: how to handle a read signal while a read is already in progress?

#define CHAR_TO_INT(c) ((c) - 48)

void serial_in(){ //serial ISR (interrupt service routine)
#if arduino
#define READ_INT() read_int()
#define READ_STRING() read_string()
#define READ_INT_ARRAY() read_int_array()
#define NL //nothing
#else
#define READ_INT() read_int(fp)
#define READ_STRING() read_string(fp)
#define READ_INT_ARRAY() read_int_array(fp)
#define NL (fgetc(fp) != '\n' ? printf("Error: expected newline\n") : 0)
#endif

#if ! arduino
  //set lock file so that other processes will not interrupt this one
  //while it reads in the new bytecode (ugly things happen in that case)
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "1");
  fclose(fp);
  //reset signal handler (it gets unset everytime for some reason)
  signal(SIGIO, (__sighandler_t)serial_in);
#endif

  int reading_str = false;
  void** code_array;
  void* stack[5]; //?

  //TODO: this should be reduced on the arduino and allowed to grow/shrink
  int max_jumps = 100;
  int max_labels = 100;
  int n_jumps = 0;
  int n_labels = 0;
  void*** jumps = (void***)malloc(sizeof(void*)*max_jumps);
  void** labels = (void**)malloc(sizeof(void*)*max_labels);

  int top = -1;

#if arduino
  if (receiving_serial) return;
  receiving_serial = true;
  //for serial to work, we need to re-enable interrupts
  interrupts();
#else
  char ch;
  fp = fopen(BYTECODE_IN_FILE, "r");
  if (!fp){
    SAY("ERROR: failed to open bytecode file '" BYTECODE_IN_FILE "'\n");
    DIE(1);//TODO: should return
  }
#endif
  char data;
  //number of bytecodes to read in
  //the nth bytecode should be the terminator
  int total = 0;
  int i = 0;//index of next bytecode in 'code_array'
  struct codeblock* newblock = NULL;
  struct procedure* newfunction = NULL;

  int reading_array = 0;
  int array_index = 0;
  int array_len = 0;
  int* int_array;
  void** any_array;
#define TYPE_ANY 1
#define TYPE_INT 2

  while (1){//until terminator is seen
    if (total == 0){
      //the first number specifies how many bytecodes are left
      total = (int)READ_INT();
      if (total == 0){
        return;
      }
      continue;
    }

#if arduino
    while (!Serial.available()){
      //wait.
    }
    data = Serial.read();
#else
    data = fgetc(fp);
    if (data == '\n'){
      continue;
    }
    if (data == EOF){
      printf("ERROR: no terminator in bytecode file '%s'\n", BYTECODE_IN_FILE);
      exit(1);//??
    }
#endif

    switch (reading_array){
    case 0: break;
    case TYPE_ANY:
      if (array_index >= array_len){
        //TODO:
      }
      //TODO:
      break;
    case TYPE_INT:
      if (array_index >= array_len){

      }
      break;
    default:
      SAY("Error: unknown array type"); DIE(1);
    }
    //#define REGISTER code_array[++i] = &int_regs[*(++curr)]
#define POP stack[top--]
#define PUSH(x) stack[++top] = (x)

    //#char op = CHAR_TO_INT(data);
    char op = data;
    if (i == total && op != SOP_END){
      SAY("ERROR: file has more bytecodes then header specified\n"); DIE(1);
    }
    if (!newfunction && !newblock
        && ! (op == SOP_START_CODEBLOCK)
        && ! (op == SOP_START_FUNCTION)
        && ! (op == SOP_END)){
      SAY("ERROR: block or lambda has not been specified\n"); DIE(1);
    }

    int n;

    switch (op){
    case SOP_PING:
#if arduino
      Serial.write(SOP_ALIVE);
#else
      printf("Alive\n");
#endif
      break;
    case SOP_START_CODEBLOCK:
      //For now we are just creating and appending a new block every time
      //TODO: the next code should specify creation/replacement of a block
      //      or just its index number with the creation/replacement implied
      if (newfunction){
        //TODO: (error)
      }
      newblock = (struct codeblock*)malloc(sizeof(struct codeblock));
      init_codeblock(newblock, total);
      code_array = newblock->code;
      break;
    case SOP_START_FUNCTION:
      if (newblock){
        //TODO: error
      }

      break;
    case SOP_INT:
      NL;
      code_array[i++] = (void*) l_load_const;
      code_array[i++] = (void*) READ_INT();
      break;
    case SOP_PRIM_CALL:  //SOP_PRIM <function index> <arg count>
      NL;
      // get the primitives function pointer
      SKIP(SOP_INT, "(in case SOP_PRIM_CALL)"); NL;
      void* tmp;
      int index;
      // get the address for the label that calls with that # args
      switch (READ_INT()){
        //TODO: put all l_call_prim_N into an array and index that
        //      then we can free that array if all code is sent
      case 0:
        tmp = l_call_prim_0; break;
      case 1:
        tmp = l_call_prim_1; break;
      case 2:
        tmp = l_call_prim_2; break;
      case 3:
        tmp = l_call_prim_3; break;
      case 4:
        tmp = l_call_prim_4; break;
      case 5:
        tmp = l_call_prim_5; break;
      case 6:
        tmp = l_call_prim_6; break;
      default:
        SAY("ERROR: (current) max args to primitive is 6\n");
      }
      code_array[i++] = tmp;
      SKIP(SOP_INT, "(in case SOP_PRIM_CALL)"); NL;
      //now read in function pointer
      index = READ_INT();
      if (index < 0 || index >= n_primitives){
#if arduino
        SAY("Error: invalid index for primitives array\n");
#else
        printf("Error: invalid index for primitives array. max: %d, got %d\n",
               index, n_primitives);
        exit(1);
#endif
      }
      code_array[i++] = primitives[index];
      break;
    case OP_GLOBAL_LOAD:
      NL;
      code_array[i++] = l_load_global;
      SKIP(SOP_INT, "(in case global_load)"); NL;
      goto read_globals_index;
    case OP_GLOBAL_STORE:
      NL;
      code_array[i++] = l_store_global;
      SKIP(SOP_INT, "(in case global_store)"); NL;
    read_globals_index:
      index = READ_INT();
      if (index < 0 || index > max_globals){
        SAY("Error: (op_load_global) invalid global variable index\n"); DIE(1);
      }
      code_array[i++] = (void*)index;
      break;
    case OP_ADD:
      NL;
      code_array[i++] = l_add;
      break;
    case OP_SUB:
      NL;
      code_array[i++] = l_sub;
      break;
    case OP_LT:
      NL;
      code_array[i++] = l_lt;
      break;
    case OP_RETURN:
      NL;
      code_array[i++] = l_ret;
      break;
    case OP_POP:
      NL;
      code_array[i++] = l_pop;
      break;
    case SOP_STR:
      NL;
      code_array[i++] = (void*) l_load_const;
      code_array[i++] = (void*) READ_STRING();
      break;
#if include_lists
    case OP_LIST:
      NL;
      code_array[i++] = l_list;
      SKIP(SOP_INT, "(in case op_list)"); NL;
      n = READ_INT();
      if (n <= 0){
        SAY("Error: (op_list) invalid length"); DIE(1);
      }
      //TODO: give warning if we don't currently have enough memory
      code_array[i++] = (void*)n;
      break;
#endif
    case OP_IF:
      NL;
      code_array[i++] = l_if;
      break;
    case OP_JUMP:
      NL;
      code_array[i++] = l_jump;
      if (n_jumps > max_jumps-1){ //-1 because we add 2 items each time
        //TODO: extend the jumps array
        SAY("Error: max jumps exceeded\n"); DIE(1);
      }
      jumps[n_jumps++] = &code_array[i++];
      SKIP(SOP_INT, "(in case op_jump)"); NL;
      jumps[n_jumps++] = (void**)READ_INT(); //TODO: store them in a separate array
      break;
    case SOP_LABEL:
      NL;
      SKIP(SOP_INT, "(in case op_label)"); NL;
      index = READ_INT();
      if (index < 0){
        SAY("Error: invalid label index\n");  DIE(1);
      }
      if (index > max_labels){
        //TODO: extend the labels array
        SAY("Error: max labels exceeded\n");  DIE(1);
      }
      labels[index] = &code_array[i];
      break;
    case SOP_NULL:
      NL;
      code_array[i++] = (void*) l_load_const;
      code_array[i++] = NULL;
      break;
    case SOP_ARRAY:
      NL;
      //TODO:
      break;
    case SOP_INT_ARRAY:
      NL;
      code_array[i++] = (void*) l_load_const;
      //TODO
      //code_array[i++] = READ_INT_ARRAY();
      break;
    case SOP_END:
      //TODO: reset jump/label variables at start of block/function transfer
      if (newblock || newfunction){
        /*
          printf("code_array = %d\n", code_array);
          printf("before label conversion:\n");
          for (int k =0;k< i;k++){
          printf("%d, ", code_array[k]);
          }
        */
        for (int j=0; j < n_jumps; j+=2){
          *(jumps[j]) = labels[(int)jumps[j+1]];
        }
        /*
          printf("\nafter conversion:\n:");
          for (int k =0;k< i;k++){
          printf("%d, ", code_array[k]);
          }
          printf("\n");
        */
      }
      if (newblock){
        //printf("appending new codeblock\n");
        code_array[i] = l_end_of_block;
        append_codeblock(newblock);
        newblock = NULL;
        code_array = NULL;
      }else if (newfunction){
        //TODO:
      }else{
#if !arduino
        fp = fopen(lockfile, "w");
        fprintf(fp, "0");
        fclose(fp);
#endif
        return; //end of file
      }
      break;
    default: //bytecode
      //TODO:
#if arduino
      SAY("ERROR: unrecognized bytecode\n");
#else
      printf("ERROR: unrecognized bytecode: '%d'\n", op);
#endif
    }
  }
#if arduino
  receiving_serial = false;
#endif
}
