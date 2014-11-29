
#define arduino 0
#define SERIAL_INTR_PIN 0
#define DEBUG 0

////////////////////////////////////////////////////////////////////////////////
#include <stdio.h>

#define NEXT(code) goto *(*code++)

#if DEBUG
#define D(...) printf(__VA_ARGS__);
#else
#define D(...)
#endif

#define ADDII 1 //int,int,target
#define ADDI 2 //a:int, int,target
#define ADD 3 //a:int, b:int, target
#define RET 4
#define PRINT_INT 5 //source:int

typedef struct string{
  int len;
  char* s;
}string;

typedef struct lambda{
  int nargs;
  void* code;
}lambda;

void serial_in();

// 'setup' and 'loop' are used so that this code will
// work with the standard Arduino IDE as well

void setup(void){
  ///// setup serial
#if arduino 
  pinMode(2, INPUT);
  digitalWrite(2, LOW);
  Serial.begin(9600);
  attachInterrupt(SERIAL_INTR_PIN, serial_in, CHANGE);
#endif
}

void loop (){
    
  /* printf("sizeof(int) = %d\n", sizeof(int)); */
  /* printf("sizeof(int) = %d\n", sizeof(long)); */
  /* printf("sizeof(int) = %d\n", sizeof(int*)); */
  /* printf("sizeof(void*) = %d\n", sizeof(void*)); */

  /*
    tmp char[50];
    sprintf(tmp"sizeof(int) = %d", sizeof(int));
    Serial.println(tmp);
    sprintf(tmp,"sizeof(int) = %d", sizeof(long));
    Serial.println(tmp);
    sprintf(tmp,"sizeof(int) = %d", sizeof(int*));
    Serial.println(tmp);
    sprintf(tmp,"sizeof(void*) = %d", sizeof(void*));
  */

  long byte_code[100] = {ADDII,
                         5,
                         6,
                         0, //register 0
                         PRINT_INT,
                         0,
                         RET,
                         0};

  void* code_array[100];
  static int int_regs[8];
  static char* char_regs[8];

  long i,*curr;
  for (i=0, curr = &byte_code[0]; *curr; i++,curr++){
    //TODO: each byte code needs to read in its args
    //      or some numbers will conflict with the byte codes
#define REGISTER code_array[++i] = &int_regs[*(++curr)]
    switch(*curr){
    case ADDII:
      code_array[i] = &&addii;
      code_array[++i] = (void*)*(++curr);
      code_array[++i] = (void*)*(++curr);
      REGISTER;
      break;
    case ADDI:
      code_array[i] = &&addi;
      code_array[++i] = (void*)*(++curr);
      REGISTER;
      break;
    case ADD:
      code_array[i] = &&add;
      code_array[++i] = &int_regs[*(++curr)];
      break;
    case RET:
      code_array[i] = &&ret;
      break;
    case PRINT_INT:
      code_array[i] = &&print_int;
      REGISTER;
      break;
    default:
      printf("you should really be handling this better...\n");
      code_array[i] = (void*)*curr;
    }
  }
  code_array[i] = 0;
  
  int i_top=0, c_top=0;

  void** code = &code_array[0];
  int *I = &int_regs[0];
  char **C = &char_regs[0];
  static long a,b,c,d,e,f,g;

  //prints translated code
  /*
    void** tmp = code;
    for (int i=0;*tmp; i++){
    printf("%d: %p\n",i, *tmp);
    tmp++;
    }
    printf("-done-\n");
  */

  NEXT(code);

 seti_a: //needed?
  a = (long)code[0];
  NEXT(code);
  
 set_a:
  a = (long)code[1];//*(code[1]); //TODO: fix: this was an error
  code += 2;
  NEXT(code);
 set_b:
  b = *((long*)code[1]);
  code += 2;
  NEXT(code);
 set_c:
  c = *((long*)code[1]);
  code += 2;
  NEXT(code);

 addii: //both args are immidate
  D("addii\n");
  a = (long)code[0];
  code++;
  //fall through
 addi: //right arg is immidate
  D("addi\n");
  b = (long)code[0];
  code++;
  //fall through
 add:
  D("add\n");

  *((int*)code[0]) = (a + b);
  code++;
  NEXT(code);

 ret:
#if arduino
  goto exit;
#else  
  printf("Exiting.\n");
  return;
#endif
  
 print_int:
#if arduino
  char tmp[100];
  sprintf(tmp, "%d", *((int*)code[0]));
  Serial.println(tmp);
#else
  printf("%d\n", *((int*)code[0]));
#endif
  code++;
  NEXT(code);
    
  /*
    printf("hi\n");
    printf("&&add = %p\n", &&add);
    printf("&&addii = %p\n", &&addii);
    printf("&&print_int = %p\n", &&print_int);
  
    printf("&a = %p\n", &a);
    printf("&b = %p\n", &b);
    printf("&&ret = %p\n", &&ret);
  */
  
#if arduino
 exit:
  Serial.println("Exiting");
#endif
}

#if arduino
#else
int main(){
  setup();
  loop(); //never returns
  return 0;
}
#endif

#if arduino
volatile boolean receiving_serial = false;

void serial_in(){ //serial ISR (interrupt service routine)
  int reading_str = false;
  
  if (receiving_serial) return;
  receiving_serial = true;
  //for serial to work, we need to re-enable interrupts
  interrupts();
  
  while (1){//until terminator is seen
    while (!Serial.available()){
      //wait.
    }
    char data = Serial.read();
    if (reading_str){
      if (data){
        //TODO: add to str array
      }else{
        //TODO: add null terminator
        //send string somewhere?
        reading_str = false;
      }
    }else{
      switch (data){
      case OP_STR:
        reading_str = true;
        break;
      case OP_END:
        return;
      default: //bytecode
        //TODO:
      }
    }
  }
  receiving_serial = false;
}
#endif
