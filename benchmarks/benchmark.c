#include <time.h>
#include <stdio.h>

double cps_div_1000 = CLOCKS_PER_SEC / 1000.0;   
long milliseconds(void){
  return (clock() / cps_div_1000);
}

int main(){
  int i =0;
  long start=milliseconds();
  long x,n,tmp,a,b,end,elapsed;
  while (i < 100000){
    x = 1;
    while (x < 100){
      n = x;
      a = 0;
      b = 1;
      while (n){
        tmp = a + b;
        a = b;
        b = tmp;
        n = n - 1;
      }
      x = x + 1;
    }
    i = i + 1;
  }
  end = milliseconds();
  elapsed=end-start;
  printf("elapsed time: %lu\n",elapsed);
  //inner loop with 20 iterations:
  //  100000 loops, elapsed time: 90
  //  1000000 loops elapsed time: 850
  //
  //inner loop with 100 iterations:  
  // 100000 loops, elapsed time: 2720
}
