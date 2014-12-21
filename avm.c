// AVM - Arduino Virtual Machine

#define ENABLE_PAUSES 1 //uses an additional 874 bytes
#define INCLUDE_LISTS 1
#define ARDUINO 0

#define DEBUG 0
#define DEBUG2 0

//NOTE: If these are changed, their value in maml_serial.py must be also changed.
#define BYTECODE_IN_FILE "_bc.txt"
#define NUM_TERMINATOR 'x'

////////////////////////////////////////////////////////////////////////////////

#include "_opcodes.h"

#if ! ARDUINO
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#endif


#if DEBUG
#define D(...) printf(__VA_ARGS__);
#else
#define D(...)
#endif
#if DEBUG2
#if ARDUINO
#define D2(...) serial_out(__VA_ARGS__);
#else
#define D2(...) printf(__VA_ARGS__);
#endif
#else
#define D2(...)
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

#if ARDUINO
#define DIE //?
#define SAY(words) maml_serial.write(words)
#else
#define DIE(n) exit(n)
//temporarily disable while for testing arduino
//#define SAY(words) printf(words)
#define SAY
#endif

#if ARDUINO
#define serial_out(x) maml_serial.write(x)
#else
#define serial_out(x) printf(x)
#endif

#define _DECL_(...)
#define _DEFUN_

void read_file(void);


struct string{
  int len;
  char* s;
};

struct array{
  int len;
  void** data;
};

struct i_array{
  int len;
  int* data;
};

struct procedure{
  void** code;
  int n_locals;
  struct i_array *args;
};

void init_procedure(struct procedure* fn, int code_len, int n_locals){
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
long paused = NULL;

#if ARDUINO
#include "maml_HardwareSerial.cpp"
#endif
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

void remove_codeblock(struct codeblock* block){
  if (block->prev){
    block->prev->next = block->next;
  }else{//removing head
    blockchain = block->next;
  }
  if (block->next){
    block->next->prev = block->prev;
  }else{//removing tail
    blockchain_end = block->prev;
  }
  if (n_codeblocks == 1){
    blockchain_end = blockchain = NULL;
  }
  free(block->code);
  free(block);
  n_codeblocks--;
}

#if INCLUDE_LISTS
struct node{
  void* data;
  struct node* next;
};

_DEFUN_
struct node* cons(void* d, struct node* list){
  struct node* _new = (struct node*)malloc(sizeof(struct node));
  _new->data = d;
  _new->next = list;
  return _new;
}
_DEFUN_
void* car(struct node* list){
  return list->data;
}
_DEFUN_
struct node* cdr(struct node* list){
  return list->next;
}
#endif


// 'setup' and 'loop' are used so that this code will
// work with the standard Arduino IDE as well

void loop();

#if !ARDUINO
char* lockfile;
#endif

void** globals; //array of global variables
int max_globals = 20; //TODO: maml.env.py needs to know about this
                      //      OR: have serial_in() check and resize!
void** locals; // array of local variables. In global scope, this points
               // to the 'globals' array.
int n_locals;
struct frame *current_frame = NULL;

//if these names are changed, also change them in process_primitives.el
void** primitives; //this is filled by auto-generated code in _prim.c
int n_primitives;


#include "primitives.c"
#if ARDUINO
#include "arduino_only_primitives.c"
#else
#include "desktop_only_primitives.c"
#endif

////////////////////////////////////////////////////////////////////////////////
//variables used by 'byte_in' to build input

enum Reading_state {done, integer, string, int_array, array, code};
Reading_state reading_state;
int expected_length;
void** code_array;
int code_i = 0;//index of next bytecode in 'code_array'
int n_jumps;
int n_labels;
//TODO: this should be reduced on the arduino and allowed to grow/shrink
int max_jumps = 100;
int max_labels = 100;
void*** jumps;
void** labels;

struct codeblock* newblock = NULL;
struct procedure* newfunction = NULL;

int reading_length = false;

void** input_stack;
unsigned char input_stack_top = 0;
#define INPUT_STACK_PUSH(x) input_stack[input_stack_top++] = (void*)(x)
#define INPUT_STACK_POP() input_stack[--input_stack_top]

//TODO: these can can be combined to reduce memory
//for reading strings
struct string* in_string_struct;
int in_string_len;
char* in_string_s;
//for reading numbers
char* in_integer;
int in_integer_i;
//for reading int arrays
int* in_int_array;
int in_array_len;
struct i_array* in_int_array_struct;

//the main program stack
void** stack;
int top = -1;//index of the top item on the stack

void setup(void){
#include "_prim.c"

  blockchain = NULL;
  blockchain_end = NULL;
  n_codeblocks = 0;
  locals = globals = (void**)malloc(sizeof(void*)*max_globals);
  n_locals = max_globals;
  jumps = (void***)malloc(sizeof(void*)*max_jumps);
  labels =  (void**)malloc(sizeof(void*)*max_labels);
  in_integer = (char*) malloc(sizeof(char)*10); //TODO: this is dumb
  input_stack = (void**)malloc(sizeof(void*)*5);//?
  stack = (void**)malloc(sizeof(void*)*10);//?
  //initialize global frame
  current_frame = (struct frame*)malloc(sizeof(struct frame));
  current_frame->locals = globals;
  current_frame->n_locals = max_globals;
  current_frame->next = current_frame->prev = NULL;

#if ARDUINO // setup signal interrupt
  maml_serial.begin(9600);
#else
  printf("Initializing avm...\n");
  lockfile = (char*)malloc(sizeof(char)*15);
  sprintf(lockfile, "%d.lock", getpid());
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "0");
  fclose(fp);

  signal(SIGIO, (__sighandler_t)read_file);
  //TODO: catch kill signal and remove lock file

#endif

  loop();// variables are initialized the first time loop is called

#if ARDUINO
  maml_serial.print("Ready.\n\n");
#else
  printf("Ready.\n\n");
#endif
}
char initialized = false;

static void** entry_table;
static void** prim_call_entry_table;

#if ENABLE_PAUSES
void _d(void){
  delay(500);
}
#define NEXT(code) while (paused){_d();}; goto *(*code++)
#else
#define NEXT(code) goto *(*code++)
#endif

void loop (){
  if (!initialized){
#include "_entrytable.h"
    initialized = true;
    return;
  }
  if (!blockchain) return;//no bytecode yet

  //this part happens only once
  struct codeblock* current_block = blockchain;
  void** code = current_block->code;

  int n;
  struct node *list = NULL;
  struct node *tmp;

  NEXT(code);

 op_const:
  D("loading const: %d\n", *code);
  stack[++top] = *code;
  code++;
  NEXT(code);
 call_prim_0:
  D("call_0\n");
  stack[++top] = ((void* (*)(void))(*code++))();
  //stack[top] = ((void* (*)(void*))(*code++))(stack[top]);
  NEXT(code);
 call_prim_1:
  D("call_0\n");
  stack[top] = ((void* (*)(void*))(*code++))(stack[top]);
  NEXT(code);
 call_prim_2:
  //// define _ and S////////////////////////////////////////////
#define _ void*
#define S(i) stack[top+i]
  D("call_2\n");
  top -= 1;
  stack[top] = ((_ (*)(_, _))(*code++))(S(0), S(1));
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
 op_return:
  NEXT(code);
 op_global_load:
  D("load_global\n");
  stack[++top] = globals[(long)*code++];
  NEXT(code);
 op_global_store:
  D("store_global\n");
  globals[(long)*code++] = stack[top--];
  NEXT(code);
 op_local_load:
  D("load_local\n");
  stack[++top] = locals[(long)*code++];
  NEXT(code);
 op_local_store:
  D("store_local\n");
  //::? (int)
  locals[(long)*code++] = stack[top--];
  NEXT(code);
 op_if:
  D("if\n");
  if (stack[top--]){
    code+=2; //skip over the jump
  }
  NEXT(code);
 op_jump:
  code = (void**)*code;
  NEXT(code);
 op_add:
  stack[top-1] = (void*)((long)stack[top-1] + (long)stack[top--]);
  NEXT(code);
 op_sub:
  D("SUB\n");
  stack[top-1] = (void*)((long)stack[top-1] - (long)stack[top--]);
  NEXT(code);
 op_mult:
  D("mult\n");
  stack[top-1] = (void*)((long)stack[top-1] * (long)stack[top--]);
  NEXT(code);
 op_div:
  D("div\n");
  stack[top-1] = (void*)((long)stack[top-1] / (long)stack[top--]);
  NEXT(code);
 op_gt:
  stack[top-1] = (void*) ((long)(stack[top-1]) > ((long)stack[top--]));
  //stack[top-1] = (void*) ((long)(stack[top]) < ((long)stack[--top]));
  NEXT(code);
 op_lt:
  //This works on the desktop, but not the arduino(why?):
  // stack[top-1] = (void*) ((long)(stack[top]) > ((long)stack[--top]));
  stack[top-1] = (void*) ((long)(stack[top-1]) < ((long)stack[top--]));
  NEXT(code);

 op_eq:
  stack[top-1] = (void*) ((long)(stack[top-1]) == ((long)stack[top--]));
  NEXT(code);
 op_not_eq:
  stack[top-1] = (void*) ((long)(stack[top-1]) != ((long)stack[top--]));
  NEXT(code);
 op_lt_eq:
  // use > because items on stack are reversed
  stack[top-1] = (void*) ((long)(stack[top-1]) <= ((long)stack[top--]));
  NEXT(code);
 op_gt_eq:
  // use < because items on stack are reversed
  stack[top-1] = (void*) ((long)(stack[top-1]) >= ((long)stack[top--]));
  NEXT(code);
#if INCLUDE_LISTS
 op_list:
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
 op_array:
  D("array\n");
  n = (int)*code++;
  struct array* tuple = (struct array*)malloc(sizeof(struct array));
  void** tmpData = (void**)malloc(sizeof(void*)*n);
  tuple->len = n;
  while (n) {
    n--;
    tmpData[n] = stack[top--];
  }
  tuple->data = tmpData;
  stack[++top] = tuple;
  NEXT(code);
 op_next_block:
  current_block = current_block->next;
  code = current_block->code;
  NEXT(code);
 op_block_suicide:
  struct codeblock* next_block = current_block->next;
  remove_codeblock(current_block);
  if (next_block && n_codeblocks){
    code = next_block->code;
    NEXT(code);
  }
  return; //no more code blocks - keep looping until one arrives
 op_pop:
  D("POP\n");
  --top;
  NEXT(code);
}

#if ARDUINO
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

#define CHAR_TO_INT(c) ((c) - 48)


////////////////////////////////////////////////////////////////////////////////

//TODO: how to handle a read signal while a read is already in progress?


void byte_in(unsigned char c){
  /* if (c != SOP_PING){ */
  /*   serial_out("got>"); */
  /*   serial_out(c); */
  /* } */
  //processes the next byte of input

  switch (reading_state){
    ///start by reading the length of the code to be received
  case string:
    D2("reading_state:string\n");
    if (c){
      //TODO: 'in_string_len' holds the max length of 'in_string_s',
      // check that it does not exceed it
      *in_string_s++ = c;
    }else{
      *in_string_s = '\0';
      INPUT_STACK_PUSH(in_string_struct);
      reading_state = done;
      D2("read string -----> %s\n", in_string_struct->s);
    }
    return;

  case integer:
    D2("reading_state:integer\n");
    if (c != NUM_TERMINATOR){
      in_integer[in_integer_i++] = c;
    }else{
      in_integer[in_integer_i++] = '\0';
      INPUT_STACK_PUSH(atoi(in_integer));
      reading_state = done;
      D2("read integer -----> %s\n", in_integer);
    }
    return;

  case int_array:
    in_array_len--;
    *in_int_array++ = c;
    if (!in_array_len){
      INPUT_STACK_PUSH(in_int_array_struct);
      reading_state = done;
    }
    return;

  case done://c is a bytecode
    switch (c){
    case SOP_PING:
      //serial_out("SOP_PING\n");
      serial_out(SOP_ALIVE);
      return;
    case SOP_STR:
      D2("SOP_STR\n");
      reading_state = string;
      in_string_len = (int)INPUT_STACK_POP();
      D2("reading string len ===>>> %d\n", in_string_len);
      in_string_struct = (struct string*)malloc(sizeof(struct string));
      in_string_s = (char*)malloc(sizeof(char)*in_string_len+1);
      in_string_struct->s = in_string_s;
      in_string_struct->len = in_string_len;
      return;
    case SOP_INT_ARRAY:
      reading_state = int_array;
      in_array_len = (int)INPUT_STACK_POP();
      in_int_array_struct = (struct i_array*)malloc(sizeof(struct i_array));
      in_int_array = (int*)malloc(sizeof(int)*in_array_len);
      in_int_array_struct->data = in_int_array;
      in_int_array_struct->len = in_array_len;
      return;
    case OP_CONST:
      code_array[code_i++] = entry_table[OP_CONST];
      code_array[code_i++] = INPUT_STACK_POP();
      return;
    case SOP_INT:
      D2("SOP_INT\n");
      //TODO: when compiling, check that number literals are not too big.
      reading_state = integer;
      in_integer_i = 0;
      return;
    case SOP_START_CODEBLOCK:
      D2("SOP_START_CODEBLOCK\n");
      n_jumps = 0;
      n_labels = 0;
      //TODO: the length of the code block should be sent separately
      //from the 'total' value
      //serial_out("SOP_START_CODEBLOCK\n");

      //For now we are just creating and appending a new block every time
      //TODO: the next code should specify creation/replacement of a block
      //      or just its index number with the creation/replacement implied
      if (newfunction){
        SAY("Error: cannot start a codeblock while reading a function\n");
      }

      //TODO: keep track of the number of bytecodes written into
      //      code array and compare to 'expected_length' in sop_end
      expected_length = (int)INPUT_STACK_POP();
      D2("expected length --> %d\n", expected_length);
      newblock = (struct codeblock*)malloc(sizeof(struct codeblock));
      init_codeblock(newblock, expected_length);
      code_array = newblock->code;
      code_i = 0;
      return;

    case SOP_START_FUNCTION:
      {
        printf("SOP_START_FUNCTION\n");
        if (newblock){
          //TODO: error
        }

        expected_length = (int)INPUT_STACK_POP();

        newfunction = (struct procedure*)malloc(sizeof(struct procedure));
        code_array = newfunction->code = (void**)malloc(sizeof(void*)*expected_length);
        newfunction->args = (struct i_array*)INPUT_STACK_POP();

        int len = newfunction->args->len;
        int* data = newfunction->args->data;
        for (int i = 0; i < len; i++, data++){
          printf("%d: %d\n", i, *data);
        }
        //newfunction->args = READ_INT_ARRAY();
        //newfunction->
        code_array = newfunction->code;
        return;
      }
    case SOP_PRIM_CALL:  //SOP_PRIM <function index> <arg count>
      {
        D2("SOP_PRIM_CALL\n");
        // get the primitives function pointer

        void* tmp;
        int index = (int)INPUT_STACK_POP(); //pop arg count
        // get the address for the label that calls with that # args

        if (index < 0 || index > 6){
          serial_out("ERROR: (current) max args to primitive is 6\n");
          printf("index = %d\n", index);
          D2("got %d arg index\n", index);
          return;
        }
        code_array[code_i++] = prim_call_entry_table[index];

        //now read in function pointer
        index = (int)INPUT_STACK_POP();
        D2("primative function index -------> %d\n", index);
        if (index < 0 || index >= n_primitives){
#if ARDUINO
          serial_out("Error: invalid index for primitives array\n");
#else
          printf("Error: invalid index for primitives array. max: %d, got %d\n",
                 index, n_primitives);
          exit(1);
#endif
        }
        code_array[code_i++] = primitives[index];
        return;
      }
    case OP_GLOBAL_LOAD:
      SAY("OP_GLOBAL_LOAD\n");
      code_array[code_i++] = entry_table[OP_GLOBAL_LOAD];
      goto read_globals_index;
    case OP_GLOBAL_STORE:
      {
        SAY("OP_GLOBAL_STORE\n");
        code_array[code_i++] = entry_table[OP_GLOBAL_STORE];
      read_globals_index:
        int index = (int)INPUT_STACK_POP();
        if (index < 0 || index > max_globals){
          SAY("Error: (op_load_global) invalid global variable index\n"); DIE(1);
        }
        code_array[code_i++] = (void*)index;
        return;
      }
#if INCLUDE_LISTS
    case OP_LIST:
      {
        SAY("OP_LIST\n");
        code_array[code_i++] = entry_table[OP_LIST];
        int n = (int)INPUT_STACK_POP();
        if (n <= 0){
          SAY("Error: (op_list) invalid length"); DIE(1);
        }
        //TODO: give warning if we don't currently have enough memory
        code_array[code_i++] = (void*)n;
        return;
      }
#endif
    case OP_ARRAY:
      {
        SAY("OP_ARRAY\n");
        code_array[code_i++] = entry_table[OP_ARRAY];
        int n = (int)INPUT_STACK_POP();
        if ( n <= 0 ) {
          SAY("Error: (op_array) invalid length"); DIE(1);
        }
        code_array[code_i++] = (void*)n;
        return;
      }
    case OP_JUMP:
      SAY("OP_JUMP\n");
      code_array[code_i++] = entry_table[OP_JUMP];
      if (n_jumps > max_jumps-1){ //-1 because we add 2 items each time
        //TODO: extend the jumps array
        SAY("Error: max jumps exceeded\n"); DIE(1);
      }
      jumps[n_jumps++] = &code_array[code_i++];
      jumps[n_jumps++] = (void**)INPUT_STACK_POP(); //TODO: store them in a separate array
      return;
    case SOP_LABEL:
      {
        SAY("SOP_LABEL\n");
        int index = (int)INPUT_STACK_POP();
        if (index < 0){
          SAY("Error: invalid label index\n");  DIE(1);
        }
        if (index > max_labels){
          //TODO: extend the labels array
          SAY("Error: max labels exceeded\n");  DIE(1);
        }
        labels[index] = &code_array[code_i];
        return;
      }
    case SOP_NULL:
      SAY("SOP_NULL\n");
      code_array[code_i++] = entry_table[OP_CONST];
      code_array[code_i++] = NULL;
      return;
    case SOP_ARRAY:
      SAY("SOP_ARRAY\n");
      //TODO:
      return;
    case SOP_END:
      D2("SOP_END\n");
      //TODO: reset jump/label variables at start of block/function transfer
      if (newblock || newfunction){
        /*
          printf("code_array = %d\n", code_array);
          printf("before label conversion:\n");
          for (int k =0;k< i;k++){
          printf("%d, ", code_array[k]);
          }
        */
        for (int i=0; i < n_jumps; i+=2){
          *(jumps[i]) = labels[(int)jumps[i+1]];
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
        append_codeblock(newblock);
        D2("appended codeblock\n");

        newblock = NULL;
        code_array = NULL;

      }else if (newfunction){
        int index = (int)INPUT_STACK_POP();
        if (index < 0 || index > max_globals){
          SAY("Error: (storing function) invalid index\n"); DIE(1);
        }
        //printf("inserting function into global index %d\n", index);
        globals[index] = (void*)newfunction;
        newfunction = NULL;
      }else{
        D2("end of input\n");
      }
      //serial_out("received:");
      /* for (int i = 0; i < code_i;i++){ */
      /*   serial_out((int)(code_array[i])); */
      /* } */
      /* delay(10000); */
      return;
    case SOP_SET:
      {
        int index = (int)INPUT_STACK_POP();
        if (index < 0 || index > max_globals){
          SAY("Error: (op_set) invalid global variable index\n"); DIE(1);
        }
        globals[index] = INPUT_STACK_POP();
        return;
      }
    case SOP_GET:
      {
        int index = (int)INPUT_STACK_POP();
        if (index < 0 || index > max_globals){
          SAY("Error: (op_get) invalid global variable index\n"); DIE(1);
        }
#if ARDUINO
        maml_serial.println((int)globals[index]);
#else
        printf("%d\n",(int)globals[index]);
#endif
        return;
      }
    case SOP_PAUSE:
      paused = true;
      return;
    case SOP_RESUME:
      serial_out("resuming...\n");
      paused = NULL;
      return;
    case SOP_DUMP_STACK:
      for (int i = 0; i <= top; i++){
#if ARDUINO
        maml_serial.print(i);
        maml_serial.print(" ");
        maml_serial.println((int)stack[i]);
#else
        printf("%d %d\n", i, (int)stack[i]);
#endif
      }
      return;

    case OP_IF:
    case OP_ADD:
    case OP_MULT:
    case OP_SUB:
    case OP_DIV:
    case OP_GT:
    case OP_LT:
    case OP_EQ:
    case OP_NOT_EQ:
    case OP_LT_EQ:
    case OP_GT_EQ:
    case OP_RETURN:
    case OP_NEXT_BLOCK:
    case OP_BLOCK_SUICIDE:
    case OP_POP:
      code_array[code_i++] = entry_table[c];
      return;

    default: //bytecode
      //TODO:
      serial_out("ERROR: unrecognized bytecode\n");
      serial_out("unknown bytecode = ");
      serial_out(c);
      serial_out("\n");

    }//end bytecode switch
    return;
  default:
    serial_out("error: invalid reading state\n");

  }//end reading state switch

  //TODO: opcode for verifying that everything is sent to the Arduino
  //      and another for resetting the reading state
  //      -> how can that be possible? => use values outside standard ascii range?

  /*
    char op = data;
    if (i == total && op != SOP_END){
    //if (i == 17 && op != SOP_END){
    SAY("ERROR: file has more bytecodes then header specified\n"); DIE(1);
    }
    if (!newfunction && !newblock
    && ! (op == SOP_START_CODEBLOCK)
    && ! (op == SOP_START_FUNCTION)
    && ! (op == SOP_END)){
    SAY("ERROR: block or lambda has not been specified\n"); DIE(1);
    }
  */
}


#if !ARDUINO
void read_file(void){
  //set lock file so that other processes will not interrupt this one
  //while it reads in the new bytecode (ugly things happen in that case)
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "1");
  fclose(fp);
  //reset signal handler (it gets unset everytime for some reason)

  char ch;
  fp = fopen(BYTECODE_IN_FILE, "r");
  if (!fp){
    printf("ERROR: failed to open bytecode file '" BYTECODE_IN_FILE "'\n");
    exit(1);//TODO: should return
  }
  //read until file is empty

  while ((ch = fgetc(fp)) != EOF){
    byte_in(ch);
  }

  fp = fopen(lockfile, "w");
  fprintf(fp, "0");
  fclose(fp);

  signal(SIGIO, (__sighandler_t)read_file);
}
#endif


//compiled size in bytes
//10,042
//11,898
//9,626
//9,782
//11,366
//13,362
//14,072 <-- this was the change to using from variables to entry_table
//15,174
//Global variables use 812 bytes (9%) of dynamic memory
//794
//858
//880
//1,038
