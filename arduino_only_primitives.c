

_DECL_(pinMode, int)
_DECL_(digitalWrite, int, int)
_DECL_(delayMicroseconds, int)
_DECL_(analogWrite, int, int)
_DECL_(millis, -> int)
_DECL_(delay, int)



_DEFUN_(int, -> none)
void _delay(int n){
  delay(n);
}

_DEFUN_(-> int)
int get_distance(){
  //returns distance in centimeters from a ping sensor setup on pin 53
  pinMode(53, OUTPUT);
  digitalWrite(53, LOW);
  delayMicroseconds(2);
  digitalWrite(53, HIGH);
  delayMicroseconds(5);
  digitalWrite(53, LOW);

  pinMode(53, INPUT);

  return pulseIn(53, HIGH) / 29 / 2;
}

