from time import time;

def milliseconds():
    return time()*1000.0
    
#calculate the first 20 Fibonacci numbers 1000 times
i =0;
start=milliseconds()
while i < 100000:
    x = 1
    while x < 100:
        n = x;
        a = 0;
        b = 1
        while n:
            tmp = a + b
            a = b
            b = tmp
            n = n - 1
        x = x + 1
    i = i + 1
end = milliseconds()
elapsed=end-start
print("elapsed time:",elapsed)

# inner loop with 20 iterations:
# 
# 100000 loops: elapsed time: 7637
#  
# inner loop with 100 iterations:
# 100000 loops: elapsed time: 182373
#    
