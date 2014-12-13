/*
  includes code from files:
  - HardwareSerial.h
  - HardwareSerial_private.h
  - HardwareSerial.cpp
  - HardwareSerial0.cpp
  taken from: Arduino-1.5.8/hardware/arduino/avr/cores/arduino
*/

#include "Arduino.h"
#include "Stream.h"
#include "wiring_private.h"
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////
//  HardwareSerial.h 

// Define constants and variables for buffering incoming serial data.  We're
// using a ring buffer (I think), in which head is the index of the location
// to which to write the next incoming character and tail is the index of the
// location from which to read.
#if !(defined(SERIAL_TX_BUFFER_SIZE) && defined(SERIAL_RX_BUFFER_SIZE))
#if (RAMEND < 1000)
#define SERIAL_TX_BUFFER_SIZE 16
#define SERIAL_RX_BUFFER_SIZE 16
#else
#define SERIAL_TX_BUFFER_SIZE 64
#define SERIAL_RX_BUFFER_SIZE 64
#endif
#endif
#if (SERIAL_TX_BUFFER_SIZE>256)
typedef uint16_t tx_buffer_index_t;
#else
typedef uint8_t tx_buffer_index_t;
#endif
#if  (SERIAL_RX_BUFFER_SIZE>256)
typedef uint16_t rx_buffer_index_t;
#else
typedef uint8_t rx_buffer_index_t;
#endif

// Define config for Serial.begin(baud, config);
#define SERIAL_5N1 0x00
#define SERIAL_6N1 0x02
#define SERIAL_7N1 0x04
#define SERIAL_8N1 0x06
#define SERIAL_5N2 0x08
#define SERIAL_6N2 0x0A
#define SERIAL_7N2 0x0C
#define SERIAL_8N2 0x0E
#define SERIAL_5E1 0x20
#define SERIAL_6E1 0x22
#define SERIAL_7E1 0x24
#define SERIAL_8E1 0x26
#define SERIAL_5E2 0x28
#define SERIAL_6E2 0x2A
#define SERIAL_7E2 0x2C
#define SERIAL_8E2 0x2E
#define SERIAL_5O1 0x30
#define SERIAL_6O1 0x32
#define SERIAL_7O1 0x34
#define SERIAL_8O1 0x36
#define SERIAL_5O2 0x38
#define SERIAL_6O2 0x3A
#define SERIAL_7O2 0x3C
#define SERIAL_8O2 0x3E

class maml_HardwareSerial : public Stream
{
protected:
  volatile uint8_t * const _ubrrh;
  volatile uint8_t * const _ubrrl;
  volatile uint8_t * const _ucsra;
  volatile uint8_t * const _ucsrb;
  volatile uint8_t * const _ucsrc;
  volatile uint8_t * const _udr;
  // Has any byte been written to the UART since begin()
  bool _written;

  volatile rx_buffer_index_t _rx_buffer_head;
  volatile rx_buffer_index_t _rx_buffer_tail;
  volatile tx_buffer_index_t _tx_buffer_head;
  volatile tx_buffer_index_t _tx_buffer_tail;

  // Don't put any members after these buffers, since only the first
  // 32 bytes of this struct can be accessed quickly using the ldd
  // instruction.
  unsigned char _rx_buffer[SERIAL_RX_BUFFER_SIZE];
  unsigned char _tx_buffer[SERIAL_TX_BUFFER_SIZE];

public:
  inline maml_HardwareSerial(
                             volatile uint8_t *ubrrh, volatile uint8_t *ubrrl,
                             volatile uint8_t *ucsra, volatile uint8_t *ucsrb,
                             volatile uint8_t *ucsrc, volatile uint8_t *udr);
  void begin(unsigned long baud) { begin(baud, SERIAL_8N1); }
  void begin(unsigned long, uint8_t);
  void end();
  virtual int available(void);
  virtual int peek(void);
  virtual int read(void);
  int availableForWrite(void);
  virtual void flush(void);
  virtual size_t write(uint8_t);
  inline size_t write(unsigned long n) { return write((uint8_t)n); }
  inline size_t write(long n) { return write((uint8_t)n); }
  inline size_t write(unsigned int n) { return write((uint8_t)n); }
  inline size_t write(int n) { return write((uint8_t)n); }
  using Print::write; // pull in write(str) and write(buf, size) from Print
  operator bool() { return true; }

  // Interrupt handlers - Not intended to be called externally
  inline void _rx_complete_irq(void);
  void _tx_udr_empty_irq(void);
};


/*
#if defined(UBRRH) || defined(UBRR0H)
extern maml_HardwareSerial Serial;
#define HAVE_HWSERIAL0
#endif
#if defined(UBRR1H)
extern maml_HardwareSerial Serial1;
#define HAVE_HWSERIAL1
#endif
#if defined(UBRR2H)
extern maml_HardwareSerial Serial2;
#define HAVE_HWSERIAL2
#endif
#if defined(UBRR3H)
extern maml_HardwareSerial Serial3;
#define HAVE_HWSERIAL3
#endif
*/
extern void serialEventRun(void) __attribute__((weak));



////////////////////////////////////////////////////////////////////////////////
/// HardwareSerial_private.h 

// this next line disables the entire HardwareSerial.cpp, 
// this is so I can support Attiny series and any other chip without a uart
#if defined(HAVE_HWSERIAL0) || defined(HAVE_HWSERIAL1) || defined(HAVE_HWSERIAL2) || defined(HAVE_HWSERIAL3)

// Ensure that the various bit positions we use are available with a 0
// postfix, so we can always use the values for UART0 for all UARTs. The
// alternative, passing the various values for each UART to the
// HardwareSerial constructor also works, but makes the code bigger and
// slower.
#if !defined(TXC0)
#if defined(TXC)
// Some chips like ATmega8 don't have UPE, only PE. The other bits are
// named as expected.
#if !defined(UPE) && defined(PE)
#define UPE PE
#endif
// On ATmega8, the uart and its bits are not numbered, so there is no TXC0 etc.
#define TXC0 TXC
#define RXEN0 RXEN
#define TXEN0 TXEN
#define RXCIE0 RXCIE
#define UDRIE0 UDRIE
#define U2X0 U2X
#define UPE0 UPE
#define UDRE0 UDRE
#elif defined(TXC1)
// Some devices have uart1 but no uart0
#define TXC0 TXC1
#define RXEN0 RXEN1
#define TXEN0 TXEN1
#define RXCIE0 RXCIE1
#define UDRIE0 UDRIE1
#define U2X0 U2X1
#define UPE0 UPE1
#define UDRE0 UDRE1
#else
#error No UART found in HardwareSerial.cpp
#endif
#endif // !defined TXC0

// Check at compiletime that it is really ok to use the bit positions of
// UART0 for the other UARTs as well, in case these values ever get
// changed for future hardware.
#if defined(TXC1) && (TXC1 != TXC0 || RXEN1 != RXEN0 || RXCIE1 != RXCIE0 || \
                      UDRIE1 != UDRIE0 || U2X1 != U2X0 || UPE1 != UPE0 || \
                      UDRE1 != UDRE0)
#error "Not all bit positions for UART1 are the same as for UART0"
#endif
#if defined(TXC2) && (TXC2 != TXC0 || RXEN2 != RXEN0 || RXCIE2 != RXCIE0 || \
                      UDRIE2 != UDRIE0 || U2X2 != U2X0 || UPE2 != UPE0 || \
                      UDRE2 != UDRE0)
#error "Not all bit positions for UART2 are the same as for UART0"
#endif
#if defined(TXC3) && (TXC3 != TXC0 || RXEN3 != RXEN0 || RXCIE3 != RXCIE0 || \
                      UDRIE3 != UDRIE0 || U3X3 != U3X0 || UPE3 != UPE0 || \
                      UDRE3 != UDRE0)
#error "Not all bit positions for UART3 are the same as for UART0"
#endif

// Constructors ////////////////////////////////////////////////////////////////

maml_HardwareSerial::maml_HardwareSerial(
                                         volatile uint8_t *ubrrh, volatile uint8_t *ubrrl,
                                         volatile uint8_t *ucsra, volatile uint8_t *ucsrb,
                                         volatile uint8_t *ucsrc, volatile uint8_t *udr) :
  _ubrrh(ubrrh), _ubrrl(ubrrl),
  _ucsra(ucsra), _ucsrb(ucsrb), _ucsrc(ucsrc),
  _udr(udr),
  _rx_buffer_head(0), _rx_buffer_tail(0),
  _tx_buffer_head(0), _tx_buffer_tail(0)
{
}

// Actual interrupt handlers //////////////////////////////////////////////////////////////

void maml_HardwareSerial::_rx_complete_irq(void)
{
  if (bit_is_clear(*_ucsra, UPE0)) {
    // No Parity error, read byte and store it in the buffer if there is
    // room
    unsigned char c = *_udr;
    rx_buffer_index_t i;
    i = (unsigned int)(_rx_buffer_head + 1) % SERIAL_RX_BUFFER_SIZE;
    
    // if we should be storing the received character into the location
    // just before the tail (meaning that the head would advance to the
    // current location of the tail), we're about to overflow the buffer
    // and so we don't write the character or advance the head.      
    if (i != _rx_buffer_tail) {
      _rx_buffer[_rx_buffer_head] = c;
      _rx_buffer_head = i;
    }
  } else {
    // Parity error, read byte but discard it
    *_udr; //wtf?
  };
}

#endif // whole file

////////////////////////////////////////////////////////////////////////////////
// HardwareSerial.cpp


// this next line disables the entire HardwareSerial.cpp, 
// this is so I can support Attiny series and any other chip without a uart
#if defined(HAVE_HWSERIAL0) || defined(HAVE_HWSERIAL1) || defined(HAVE_HWSERIAL2) || defined(HAVE_HWSERIAL3)

// SerialEvent functions are weak, so when the user doesn't define them,
// the linker just sets their address to 0 (which is checked below).
// The Serialx_available is just a wrapper around Serialx.available(),
// but we can refer to it weakly so we don't pull in the entire
// HardwareSerial instance if the user doesn't also refer to it.
#if defined(HAVE_HWSERIAL0)
  void serialEvent() __attribute__((weak));
  bool Serial0_available() __attribute__((weak));
#endif

#if defined(HAVE_HWSERIAL1)
  void serialEvent1() __attribute__((weak));
  bool Serial1_available() __attribute__((weak));
#endif

#if defined(HAVE_HWSERIAL2)
  void serialEvent2() __attribute__((weak));
  bool Serial2_available() __attribute__((weak));
#endif

#if defined(HAVE_HWSERIAL3)
  void serialEvent3() __attribute__((weak));
  bool Serial3_available() __attribute__((weak));
#endif

void serialEventRun(void)
{
#if defined(HAVE_HWSERIAL0)
  if (Serial0_available && serialEvent && Serial0_available()) serialEvent();
#endif
#if defined(HAVE_HWSERIAL1)
  if (Serial1_available && serialEvent1 && Serial1_available()) serialEvent1();
#endif
#if defined(HAVE_HWSERIAL2)
  if (Serial2_available && serialEvent2 && Serial2_available()) serialEvent2();
#endif
#if defined(HAVE_HWSERIAL3)
  if (Serial3_available && serialEvent3 && Serial3_available()) serialEvent3();
#endif
}

// Actual interrupt handlers //////////////////////////////////////////////////////////////

void maml_HardwareSerial::_tx_udr_empty_irq(void)
{
  // If interrupts are enabled, there must be more data in the output
  // buffer. Send the next byte
  unsigned char c = _tx_buffer[_tx_buffer_tail];
  _tx_buffer_tail = (_tx_buffer_tail + 1) % SERIAL_TX_BUFFER_SIZE;

  *_udr = c;

  // clear the TXC bit -- "can be cleared by writing a one to its bit
  // location". This makes sure flush() won't return until the bytes
  // actually got written
  sbi(*_ucsra, TXC0);

  if (_tx_buffer_head == _tx_buffer_tail) {
    // Buffer empty, so disable interrupts
    cbi(*_ucsrb, UDRIE0);
  }
}

// Public Methods //////////////////////////////////////////////////////////////

void maml_HardwareSerial::begin(unsigned long baud, byte config)
{
  // Try u2x mode first
  uint16_t baud_setting = (F_CPU / 4 / baud - 1) / 2;
  *_ucsra = 1 << U2X0;

  // hardcoded exception for 57600 for compatibility with the bootloader
  // shipped with the Duemilanove and previous boards and the firmware
  // on the 8U2 on the Uno and Mega 2560. Also, The baud_setting cannot
  // be > 4095, so switch back to non-u2x mode if the baud rate is too
  // low.
  if (((F_CPU == 16000000UL) && (baud == 57600)) || (baud_setting >4095))
  {
    *_ucsra = 0;
    baud_setting = (F_CPU / 8 / baud - 1) / 2;
  }

  // assign the baud_setting, a.k.a. ubrr (USART Baud Rate Register)
  *_ubrrh = baud_setting >> 8;
  *_ubrrl = baud_setting;

  _written = false;

  //set the data bits, parity, and stop bits
#if defined(__AVR_ATmega8__)
  config |= 0x80; // select UCSRC register (shared with UBRRH)
#endif
  *_ucsrc = config;
  
  sbi(*_ucsrb, RXEN0);
  sbi(*_ucsrb, TXEN0);
  sbi(*_ucsrb, RXCIE0);
  cbi(*_ucsrb, UDRIE0);
}

void maml_HardwareSerial::end()
{
  // wait for transmission of outgoing data
  while (_tx_buffer_head != _tx_buffer_tail)
    ;

  cbi(*_ucsrb, RXEN0);
  cbi(*_ucsrb, TXEN0);
  cbi(*_ucsrb, RXCIE0);
  cbi(*_ucsrb, UDRIE0);
  
  // clear any received data
  _rx_buffer_head = _rx_buffer_tail;
}

int maml_HardwareSerial::available(void)
{
  return (int)(SERIAL_RX_BUFFER_SIZE + _rx_buffer_head - _rx_buffer_tail) % SERIAL_RX_BUFFER_SIZE;
}

int maml_HardwareSerial::peek(void)
{
  if (_rx_buffer_head == _rx_buffer_tail) {
    return -1;
  } else {
    return _rx_buffer[_rx_buffer_tail];
  }
}

int maml_HardwareSerial::read(void)
{
  // if the head isn't ahead of the tail, we don't have any characters
  if (_rx_buffer_head == _rx_buffer_tail) {
    return -1;
  } else {
    unsigned char c = _rx_buffer[_rx_buffer_tail];
    _rx_buffer_tail = (rx_buffer_index_t)(_rx_buffer_tail + 1) % SERIAL_RX_BUFFER_SIZE;
    return c;
  }
}

int maml_HardwareSerial::availableForWrite(void)
{
#if (SERIAL_TX_BUFFER_SIZE>256)
  uint8_t oldSREG = SREG;
  cli();
#endif
  tx_buffer_index_t head = _tx_buffer_head;
  tx_buffer_index_t tail = _tx_buffer_tail;
#if (SERIAL_TX_BUFFER_SIZE>256)
  SREG = oldSREG;
#endif
  if (head >= tail) return SERIAL_TX_BUFFER_SIZE - 1 - head + tail;
  return tail - head - 1;
}

void maml_HardwareSerial::flush()
{
  // If we have never written a byte, no need to flush. This special
  // case is needed since there is no way to force the TXC (transmit
  // complete) bit to 1 during initialization
  if (!_written)
    return;

  while (bit_is_set(*_ucsrb, UDRIE0) || bit_is_clear(*_ucsra, TXC0)) {
    if (bit_is_clear(SREG, SREG_I) && bit_is_set(*_ucsrb, UDRIE0))
	// Interrupts are globally disabled, but the DR empty
	// interrupt should be enabled, so poll the DR empty flag to
	// prevent deadlock
	if (bit_is_set(*_ucsra, UDRE0))
	  _tx_udr_empty_irq();
  }
  // If we get here, nothing is queued anymore (DRIE is disabled) and
  // the hardware finished tranmission (TXC is set).
}

size_t maml_HardwareSerial::write(uint8_t c)
{
  // If the buffer and the data register is empty, just write the byte
  // to the data register and be done. This shortcut helps
  // significantly improve the effective datarate at high (>
  // 500kbit/s) bitrates, where interrupt overhead becomes a slowdown.
  if (_tx_buffer_head == _tx_buffer_tail && bit_is_set(*_ucsra, UDRE0)) {
    *_udr = c;
    sbi(*_ucsra, TXC0);
    return 1;
  }
  tx_buffer_index_t i = (_tx_buffer_head + 1) % SERIAL_TX_BUFFER_SIZE;
	
  // If the output buffer is full, there's nothing for it other than to 
  // wait for the interrupt handler to empty it a bit
  while (i == _tx_buffer_tail) {
    if (bit_is_clear(SREG, SREG_I)) {
      // Interrupts are disabled, so we'll have to poll the data
      // register empty flag ourselves. If it is set, pretend an
      // interrupt has happened and call the handler to free up
      // space for us.
      if(bit_is_set(*_ucsra, UDRE0))
	_tx_udr_empty_irq();
    } else {
      // nop, the interrupt handler will free up space for us
    }
  }

  _tx_buffer[_tx_buffer_head] = c;
  _tx_buffer_head = i;
	
  sbi(*_ucsrb, UDRIE0);
  _written = true;
  
  return 1;
}


#endif // whole file


////////////////////////////////////////////////////////////////////////////////


#if defined(UBRRH) && defined(UBRRL)
maml_HardwareSerial maml_serial(&UBRRH, &UBRRL, &UCSRA, &UCSRB, &UCSRC, &UDR);
#else
maml_HardwareSerial maml_serial(&UBRR0H, &UBRR0L, &UCSR0A, &UCSR0B, &UCSR0C, &UDR0);
#endif


////////////////////////////////////////////////////////////////////////////////
// HardwareSerial0.cpp


// Each HardwareSerial is defined in its own file, sine the linker pulls
// in the entire file when any element inside is used. --gc-sections can
// additionally cause unused symbols to be dropped, but ISRs have the
// "used" attribute so are never dropped and they keep the
// HardwareSerial instance in as well. Putting each instance in its own
// file prevents the linker from pulling in any unused instances in the
// first place.



//#if defined(HAVE_HWSERIAL0)

#if defined(USART_RX_vect)
ISR(USART_RX_vect)
#elif defined(USART0_RX_vect)
ISR(USART0_RX_vect)
#elif defined(USART_RXC_vect)
ISR(USART_RXC_vect) // ATmega8
#else
#error "Don't know what the Data Received vector is called for Serial"
#endif
{
  maml_serial._rx_complete_irq();
}

#if defined(UART0_UDRE_vect)
ISR(UART0_UDRE_vect)
#elif defined(UART_UDRE_vect)
ISR(UART_UDRE_vect)
#elif defined(USART0_UDRE_vect)
ISR(USART0_UDRE_vect)
#elif defined(USART_UDRE_vect)
ISR(USART_UDRE_vect)
#else
#error "Don't know what the Data Register Empty vector is called for Serial"
#endif
{
  maml_serial._tx_udr_empty_irq();
}


/****
// Function that can be weakly referenced by serialEventRun to prevent
// pulling in this file if it's not otherwise used.
bool Serial0_available() {
  return Serial.available();
}
****/
//#endif // HAVE_HWSERIAL0


/*


void setup() {
  maml_serial.begin(9600);
}

void loop() {

  while (!maml_serial.available()){}
  maml_serial.write(maml_serial.read());
}


*/


// end maml_HardwareSerial.cppg
