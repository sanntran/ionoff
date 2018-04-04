#include <main.h>

#ifdef USE_PORTD_LCD
#include "lcd18f.c"
#endif

#include "tcpip/stacktsk.c"
#include "config.c"
#include "tcpclient.c"
#include "inputscan.c"

void MyMacInit() {
   MY_MAC_BYTE1 = MY_DEFAULT_MAC_BYTE1;
   MY_MAC_BYTE2 = MY_DEFAULT_MAC_BYTE2;
   MY_MAC_BYTE3 = MyEEPRom.Macs[0];
   MY_MAC_BYTE4 = MyEEPRom.Macs[1];
   MY_MAC_BYTE5 = MyEEPRom.Macs[2];
   MY_MAC_BYTE6 = MyEEPRom.Macs[3];
}

void MyIpInit() {
   int8 i;
   for (i = 0; i < 4; i++) {
      MY_SRV_IP_BYTES[i] = MyEEPRom.SrvIps[i];
   }
   
   //Direccion IP
   MY_IP_BYTE1 = MyEEPRom.Ips[0];
   MY_IP_BYTE2 = MyEEPRom.Ips[1];
   MY_IP_BYTE3 = MyEEPRom.Ips[2];
   MY_IP_BYTE4 = MyEEPRom.Ips[3];  

   //Puerta de Enlace
   MY_GATE_BYTE1 = MyEEPRom.Gates[0];
   MY_GATE_BYTE2 = MyEEPRom.Gates[1];
   MY_GATE_BYTE3 = MyEEPRom.Gates[2];
   MY_GATE_BYTE4 = MyEEPRom.Gates[3];

   //Mascara de Subred
   MY_MASK_BYTE1 = MyEEPRom.Masks[0];
   MY_MASK_BYTE2 = MyEEPRom.Masks[1];
   MY_MASK_BYTE3 = MyEEPRom.Masks[2];
   MY_MASK_BYTE4 = MyEEPRom.Masks[3];
}

void MyInputTypesInit() {
   int8 i;
   for (i = 0; i < 8; i++) {
      MY_INPUT_TYPE_BYTES[i] = MyEEPRom.InputTypes[i];
   }
}

void ConfigInit() {
   EEPReadAll();

   if (MyEEPRom.Flag != TRUE) {
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "Use Default");
#endif
      MacReset();
      ConfigReset();
      EEPWriteMac();
      EEPWriteConfig();
   }
   else {
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "Use EEPRom");
#endif
   }
   MyMacInit();
   MyIpInit();
   MyInputTypesInit();
}

void OutputHigh(int8 pin, BOOLEAN p16fRunning) {
   if (p16fRunning == FALSE) { 
      // power cut, hold system start
      delay_ms(2500);
   }   
   output_high(pin);
}

void IOsConfig() {
   
   input(IN_1);
   input(IN_2);
   input(IN_3);
   input(IN_4);
   input(IN_5);  
   input(IN_6);
   input(IN_7);
   input(IN_8);
   input(IN_RESET);
   input(IN_16F_RUNNING);

   output_low(OUT_1);
   output_low(OUT_2);
   output_low(OUT_3);
   output_low(OUT_4);
   output_low(OUT_5);
   output_low(OUT_6);
   output_low(OUT_7);
   output_low(OUT_8);
   output_low(OUT_IAM_RUNNING); 
   output_low(OUT_TCP_STATE); 
}

void OutputsRestore(void) {
   // If p16f is running, it mean this pic is reset because of WDT
   // Esle the reason of reseting is power cut
   BOOLEAN p16fRunning = FALSE; //input_state(IN_16F_RUNNING);
   if (p16fRunning == FALSE) {
      output_high(OUT_IAM_RUNNING);
      delay_ms(100);
   }
   // Restore ouput states
   if (read_eeprom(29) == TRUE) {
      OutputHigh(OUT_1, p16fRunning);
   }
   //
   if (read_eeprom(30) == TRUE) {
      OutputHigh(OUT_2, p16fRunning);
   }
   //
   if (read_eeprom(31) == TRUE) {
      OutputHigh(OUT_3, p16fRunning);
   }
   //
   if (read_eeprom(32) == TRUE) {
      OutputHigh(OUT_4, p16fRunning);
   }
   
   /*
   //
   if (read_eeprom(33) == TRUE) {
      OutputHigh(OUT_5, p16fRunning);
   }
   //
   if (read_eeprom(34) == TRUE) {
      OutputHigh(OUT_6, p16fRunning);
   }
   //
   if (read_eeprom(35) == TRUE) {
      OutputHigh(OUT_7, p16fRunning);
   }
   //
   if (read_eeprom(36) == TRUE) {
      OutputHigh(OUT_8, p16fRunning);
   }
   */
   
   if (p16fRunning == TRUE) {
      output_high(OUT_IAM_RUNNING);
   }
}

#byte UCFG = 0xF6F 
#bit UTRDIS = UCFG.3

void ResetEnc28J60(void) {
   // Check to reset ENC28J60           
   output_low(PIN_ENC_MAC_RST);
   delay_ms(100);   
   StackInit();   
   TCPClientInit();
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "ENCRestarted");
#endif
}

//
#define T1InterruptPerS   12
int8 T1InterruptCounter;
#INT_TIMER1
void timer1_isr(void) {
   // Will only do the following code if TIMER1 has interrupted

   T1InterruptCounter++;

   /*
   It is 1 seconds from reseting the Timer1 counter.
   The loop of while(TRUE) inside main() is not working. Perhaps PIC gets stuck in a method and cannot come out.
   Hmm, try to reset ENC. In many cases, the loop works again.
   */
   if (T1InterruptCounter == T1InterruptPerS) {  
#ifdef USE_PORTD_LCD
     printf(lcd_putc, "\f%s", "Crashed!ResetENC");
#endif
     ResetEnc28J60();
     clear_interrupt(INT_TIMER1);
   }

   /*
   It is 2 seconds from reseting the Timer1 counter.
   The loop of while(TRUE) inside main() is not working any more. Perhaps PIC gets stuck in a method and cannot come out.
   Hmm, there is no solution to make it work again unless using WDT to reset.
   */
   else if (T1InterruptCounter == (T1InterruptPerS * 2)) {
#ifdef USE_PORTD_LCD
     printf(lcd_putc, "\f%s", "Crashed!ResetPIC");
#endif
     clear_interrupt(INT_TIMER1);
     output_low(OUT_IAM_RUNNING);
     delay_ms(10);
     setup_wdt(WDT_ON);     
     restart_wdt();
   }
}

void Timer1Init(void) {
    setup_timer_1 ( T1_INTERNAL | T1_DIV_BY_4 ); // Internal clock and prescaler 8
    set_timer1(0);                               // Preload value
    clear_interrupt(INT_TIMER1);                 // Clear Timer1 interrupt flag bit
    enable_interrupts(INT_TIMER1);               // Enable Timer1 interrupt
    enable_interrupts(GLOBAL);
}

void main(void) {
   UTRDIS = 1; 
   setup_adc(ADC_OFF);
   setup_adc_ports(NO_ANALOGS);  
   ConfigInit();
   IOsConfig();
   delay_ms(500);
   TCPLedOff();
   OutputsRestore();
   InputsScanInit();   
   
// The LCD must be initialized before MAC, IP and Stack   
#ifdef USE_PORTD_LCD
   lcd_init();
#endif

#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "IONOFF-P8");
   //printf(lcd_putc, "\nIN_RESET %d", input_state(IN_RESET));
   //printf(lcd_putc, "\n%u.%u.%u.%u", MY_IP_BYTE1, MY_IP_BYTE2, MY_IP_BYTE3, MY_IP_BYTE4);
   printf(lcd_putc, "\n%u.%u.%u.%u", MY_SRV_IP_BYTES[0], MY_SRV_IP_BYTES[1], MY_SRV_IP_BYTES[2], MY_SRV_IP_BYTES[3]);
   //printf(lcd_putc, "\n%u.%u.%u.%u", MY_GATE_BYTE1, MY_GATE_BYTE2, MY_GATE_BYTE3, MY_GATE_BYTE4);
   
#endif
   
   Timer1Init();
   StackInit();   
   TCPClientInit();
   delay_ms(500);
   while (TRUE) {
      // Reset the timer1 counter
      T1InterruptCounter = 0;
      StackTask();
      InsListen();
      TCPClientTask();
      
      if (IsTCPClientBusy == FALSE && EncTransactionCount > 12) {         
         ResetEnc28J60();
      }
   }
}


