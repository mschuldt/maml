// AVM - Arduino Virtual Machine

//TODO: threaded code should be optional

#define arduino 0
#define SERIAL_INTR_PIN 0
#define DEBUG 0

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

#define true 1
#define false 0

typedef struct string{
  int len;
  char* s;
}string;

typedef struct lambda{
  int nargs;
  void* code;
}lambda;

typedef struct codeblock{
  int index; //index in codeblock chain
  void** code;
  int len; //length of 'code'
  struct codeblock* next;
  struct codeblock* prev;
}codeblock;

codeblock* blockchain;
codeblock* blockchain_end;
int n_codeblocks;

void init_codeblock(codeblock* block, int code_len){
  block->index = -1;
  block->len = code_len;
  //+ 1 for the END_OF_BLOCK instruction
  block->code = malloc(sizeof(void*)*(code_len + 1));
  block->prev = block->next = NULL;
}

void append_codeblock(codeblock* block){
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

//if these names are changed, also change them in process_primitives.el
void** primitives; //this is filled by auto-generated code in _prim.c
int n_primitives;
#include "primitives.c"

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
  lockfile = malloc(sizeof(char)*15);
  sprintf(lockfile, "%d.lock", getpid());
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "0");
  fclose(fp);

  signal(SIGIO, serial_in);
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
void* l_print_int;
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
static int int_regs[8];
static char* char_regs[8];

void loop (){
  if (!initialized){
    initialized = true;
    l_load_const = &&load_const;
    l_add = &&add;
    l_ret = &&ret;
    l_print_int = &&print_int;
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
    return;
  }
  if (!blockchain) return;//no bytecode yet

  //this part happens only once
  codeblock* current_block = blockchain;
  int i_top=0, c_top=0;
  void** code = current_block->code;
  int *I = &int_regs[0];
  char **C = &char_regs[0];
  static long a,b,c,d,e,f,g;

  void* stack[10];//??
  int top = -1;//index of the top item on the stack

  /*
  printf("l_load_const  = %d \n", l_load_const);
  printf("l_add  = %d \n", l_add);
  printf("l_ret  = %d \n", l_ret);
  printf("l_print_int  = %d \n", l_print_int);
  */

  //prints translated code
  /*
  printf("block->code ==>\n");
  for (int i = 0; i < current_block->len; i++){
    printf("%d: %d\n",i, *((int*)(current_block->code + i)));
  }
  printf("=====\n");
  */
  void* r_ret;

  NEXT(code);

 load_const:
  D("loading const: %d\n", *code);
  stack[++top] = *code;
  code++;
  NEXT(code);
 call_prim_0: //stack:   <function pointer> TOP
  //TODO: need to push return value onto the stack if not top level
  D("call_0\n");
  stack[++top] = ((void* (*)(void))(*code))();
  NEXT(code);
 call_prim_1:// stack: <arg> <function pointer> TOP
  D("call_1\n");
  stack[top] = ((void* (*)(void*))(*code++))(stack[top]);
  NEXT(code);
 call_prim_2: // stack: <arg1> <arg2> <function pointer> TOP
  //// define _ and S////////////////////////////////////////////
#define _ void*
#define S(i) stack[top+i]
  D("call_2\n");
  stack[--top] = ((_ (*)(_, _))(*code++))(S(0), S(1));
  NEXT(code);
 call_prim_3: // stack: <arg1> <arg2> <arg3> <function pointer> TOP
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
    //skip
    code+=2;
  }
  NEXT(code);
 jump:
  code = *code;
  NEXT(code);
 add:
  D("add\n");
  //*((int*)code[0]) = (a + b);
  stack[top-1] = (void*)((int)stack[top] + (int)stack[--top]);

  ///FOR TESTING. Because we have no other way to print yet
  /* printf("%d\n", ((int*)stack[top])); */
  /* top--; */
  /* sleep(1); */
  ////////////////////

  NEXT(code);

 end_of_block:
  current_block = current_block->next;
  code = current_block->code;
  NEXT(code);
 print_int:
  D("print_int\n");
#if arduino
  char tmp[100]; //TODO: change to use stack
  sprintf(tmp, "%d", *((int*)code[0]));
  Serial.println(tmp);
#else
  //printf("%d\n", *((int*)code[0]));
  printf("%d\n", ((int*)stack[top]));
  top--;
#endif
  NEXT(code);

 ret:
#if arduino
  goto exit;
#else
  printf("Exiting.\n");
  exit(0);
#endif

#if arduino
 exit:
  Serial.println("Exiting");
#endif
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
#define SKIP(ch, msg) if (fgetc(fp) != ch){     \
    //TODO
}
#else
#define SKIP(ch, msg) if (fgetc(fp) != ch){             \
    printf("Error: expected char '%c'"msg"\n", ch);     \
    exit(1);                                            \
  }
#endif

//TODO: equivalent functions for Arduino
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

//TODO: how to handle a read signal while a read is already in progress?

#define CHAR_TO_INT(c) ((c) - 48)

void serial_in(){ //serial ISR (interrupt service routine)
#if arduino
#define READ_INT()   //TODO
#define NL //nothing
#else
  //set lock file so that other processes will not interrupt this one
  //while it reads in the new bytecode (ugly things happen in that case)
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "1");
  fclose(fp);
  //reset signal handler (it gets unset everytime for some reason)
  signal(SIGIO, serial_in);
#define READ_INT() ((void*) read_int(fp))
#define NL (fgetc(fp) != '\n' ? printf("Error: expected newline\n") : 0)
#endif

  int reading_str = false;
  void** code_array;
  void* stack[5]; //?

  //TODO: this should be reduced on the arduino and allowed to grow/shrink
  int max_jumps = 100;
  int max_labels = 100;
  int n_jumps = 0;
  int n_labels = 0;
  void*** jumps = malloc(sizeof(void*)*max_jumps);
  void** labels = malloc(sizeof(void*)*max_labels);

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
    printf("ERROR: failed to open bytecode file '" BYTECODE_IN_FILE "'\n");
    exit(1);//TODO: should return
  }
#endif
  char data;
  //number of bytecodes to read in
  //the nth bytecode should be the terminator
  int total = 0;
  int i = 0;//index of next bytecode in 'code_array'
  codeblock* newblock = NULL;
  lambda* newlambda = NULL;

  while (1){//until terminator is seen
    if (total == 0){
      //the first number specifies how many bytecodes are left
      total = READ_INT();
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

    if (reading_str){
      if (data){
        //TODO: add to str array
      }else{
        //TODO: add null terminator
        //add string pointer to code_array
        reading_str = false;
      }
      continue;
    }
    //#define REGISTER code_array[++i] = &int_regs[*(++curr)]
#define POP stack[top--]
#define PUSH(x) stack[++top] = (x)

    //#char op = CHAR_TO_INT(data);
    char op = data;
    if (i == total && op != SOP_END){
      printf("ERROR: file has more bytecodes then header specified\n");
      exit(1);
    }
    if (!newlambda && !newblock
        && ! (op == SOP_START_CODEBLOCK)
        && ! (op == SOP_START_FUNCTION)
        && ! (op == SOP_END)){
      printf("ERROR: block or lambda has not been specified\n");
      exit(1);
    }

    switch (op){
    case SOP_START_CODEBLOCK:
      //For now we are just creating and appending a new block every time
      //TODO: the next code should specify creation/replacement of a block
      //      or just its index number with the creation/replacement implied
      if (newlambda){
        //TODO: (error)
      }
      newblock = malloc(sizeof(codeblock));
      init_codeblock(newblock, total);
      code_array = newblock->code;
      break;
    case SOP_START_FUNCTION:
      //TODO
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
      // get the address for the label that calls with that # args
      switch ((int)READ_INT()){
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
#if arduino
        //TODO
#else
        printf("ERROR: (current) max args to primitive is 6\n");
        exit(1);
#endif
      }
      code_array[i++] = tmp;
      SKIP(SOP_INT, "(in case SOP_PRIM_CALL)"); NL;
      //now read in function pointer
      int index = READ_INT();
      if (index < 0 || index >= n_primitives){
        printf("Error: invalid index for primitives array. max: %d, got %d\n",
               index, n_primitives);
        exit(1);
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
      index = (int)READ_INT();
      if (index < 0 || index > max_globals){
        printf("Error: (op_load_global) invalid global variable index\n");
      }
      code_array[i++] = (void*)index;
      break;
    case OP_ADD:
      NL;
      code_array[i++] = l_add;
      break;
    case OP_RETURN:
      NL;
      code_array[i++] = l_ret;
      break;
    case OP_PRINT_INT:
      NL;
      code_array[i++] = l_print_int;
      break;
    case OP_POP:
      NL;
      code_array[i++] = l_pop;
      break;
    case SOP_STR:
      NL;
      reading_str = true;
      break;
    case OP_IF:
      NL;
      code_array[i++] = l_if;
      break;
    case OP_JUMP:
      NL;
      code_array[i++] = l_jump;
      if (n_jumps > max_jumps-1){ //-1 because we add 2 items each time
        //TODO: extend the jumps array
        printf("Error: max jumps exceeded\n"); exit(1);
      }
      jumps[n_jumps++] = &code_array[i++];
      SKIP(SOP_INT, "(in case op_jump)"); NL;
      jumps[n_jumps++] = READ_INT();
      break;
    case SOP_LABEL:
      NL;
      SKIP(SOP_INT, "(in case op_label)"); NL;
      index = (int)READ_INT();
      if (index < 0){
        printf("Error: invalid label index\n");  exit(1);
      }
      if (index > max_labels){
        //TODO: extend the labels array
        printf("Error: max labels exceeded\n");  exit(1);
      }
      labels[index] = &code_array[i];
      break;
    case SOP_END:
      //TODO: reset jump/label variables at start of block/function transfer
      if (newblock || newlambda){
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
        newblock = code_array = NULL;
      }else if (newlambda){
        //TODO:
      }else{
        fp = fopen(lockfile, "w");
        fprintf(fp, "0");
        fclose(fp);
        return; //end of file
      }
      break;
    default: //bytecode
      //TODO:
      printf("ERROR: unrecognized bytecode: '%d'\n", op);
    }
  }
#if arduino
  receiving_serial = false;
#endif
}
