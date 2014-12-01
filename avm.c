//(local-set-key "q" (lambda () (interactive) (insert "q")))

#define arduino 0
#define SERIAL_INTR_PIN 0
#define DEBUG 0

//NOTE: If this are changed, their value in maml_serial.py must be also changed.
#define BYTECODE_IN_FILE "_bc.txt"
#define NUM_TERMINATOR 'x'

////////////////////////////////////////////////////////////////////////////////

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

#define OP_NAME             1
#define OP_ASSIGN           2
#define OP_CALL             3
#define OP_FUNCTION         4
#define OP_RETURN           5
//bin ops
#define OP_ADD              6
#define OP_MULT             7
#define OP_SUB              8
#define OP_DIV              9
#define OP_FDIV             10
#define OP_EXPT             11
#define OP_L_XOR            12
#define OP_L_OR             13
#define OP_L_AND            14
#define OP_MOD              15

//opcodes for serial protocol
#define SOP_INT             16
#define SOP_FLOAT           17
#define SOP_STR             18
#define SOP_START_CODEBLOCK 19
#define SOP_START_FUNCTION  10
#define SOP_END             21

#define OP_PRINT_INT        22  //tmp
#define OP_LOAD_CONST       23 //must be last. only exists in this file.

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

void setup(void){
  blockchain = NULL;
  blockchain_end = NULL;
  n_codeblocks = 0;

#if arduino // setup serial
  pinMode(2, INPUT);
  digitalWrite(2, LOW);
  Serial.begin(9600);
  attachInterrupt(SERIAL_INTR_PIN, serial_in, CHANGE);
#else // setup signal interrupt
  lockfile = malloc(sizeof(char)*15);
  sprintf(lockfile, "%d.lock", getpid());
  FILE *fp = fopen(lockfile, "w");
  fprintf(fp, "0");
  fclose(fp);

  signal(SIGIO, serial_in);
  //TODO: catch kill signal and remove lock file
#endif

  loop();// variables are initialized the first time loop is called
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

  NEXT(code);

 load_const:
  D("loading const: %d\n", *code);
  stack[++top] = *code;
  code++;
  NEXT(code);
 add:
  D("add\n");
  //*((int*)code[0]) = (a + b);
  stack[top-1] = (void*)((int)stack[top] + (int)stack[--top]);

  ///FOR TESTING. Because we have no other way to print yet
  printf("%d\n", ((int*)stack[top]));
  top--;
  sleep(1);
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

//TODO: equivalent functions for Arduino
int read_int(FILE* fp){
  char integer[10];
  int i=0;
  char ch;
  while ((ch = fgetc(fp)) != NUM_TERMINATOR){
    integer[i++] = ch;
    if (fgetc(fp) != '\n'){
      printf("ERROR: (read_int) expected newline after digit\n");
    }
  }
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
  void* stack[5];
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
    case SOP_STR:
      NL;
      reading_str = true;
      break;
    case SOP_END:
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
    default: //bytecode
      //TODO:
      printf("ERROR: unrecognized bytecode: '%d'\n", op);
    }
  }
#if arduino
  receiving_serial = false;
#endif
}
