#include <main.h>

void IOsConfig() {
   input(IN_P18F_RUNNING);
   input(IN_1);
   input(IN_2);
   input(IN_3);
   input(IN_4);
   input(IN_5);
   input(IN_6);
   input(IN_7);
   input(IN_8);
   
   output_low(OUT_IAM_RUNNING);
   output_low(OUT_1);
   output_low(OUT_2);
   output_low(OUT_3);
   output_low(OUT_4);
   output_low(OUT_5);
   output_low(OUT_6);
   output_low(OUT_7);
   output_low(OUT_8);   
}


void InputsScan() {
   if (input_state(IN_P18F_RUNNING) == FALSE) {
      return;
   }
   else if (input_state(OUT_IAM_RUNNING) == FALSE) {      
      output_high(OUT_IAM_RUNNING);
   }
   
   if (input_state(IN_1) == TRUE && input_state(OUT_1) == FALSE) {
      delay_ms(50);
      if (input_state(IN_1) == TRUE && input_state(OUT_1) == FALSE) {
         output_high(OUT_1);
      }
   }
   else if (input_state(IN_1) == FALSE && input_state(OUT_1) == TRUE) {
      delay_ms(50);
      if (input_state(IN_1) == FALSE && input_state(OUT_1) == TRUE) {
         output_low(OUT_1);
      }
   }
   
   if (input_state(IN_2) == TRUE && input_state(OUT_2) == FALSE) {
      delay_ms(50);
      if (input_state(IN_2) == TRUE && input_state(OUT_2) == FALSE) {
         output_high(OUT_2);
      }
   }
   else if (input_state(IN_2) == FALSE && input_state(OUT_2) == TRUE) {
      delay_ms(50);
      if (input_state(IN_2) == FALSE && input_state(OUT_2) == TRUE) {
         output_low(OUT_2);
      }
   }
   
   if (input_state(IN_3) == TRUE && input_state(OUT_3) == FALSE) {
      delay_ms(50);
      if (input_state(IN_3) == TRUE && input_state(OUT_3) == FALSE) {
         output_high(OUT_3);
      }
   }
   else if (input_state(IN_3) == FALSE && input_state(OUT_3) == TRUE) {
      delay_ms(50);
     if (input_state(IN_3) == FALSE && input_state(OUT_3) == TRUE) {
         output_low(OUT_3);
      }
   }
   
   if (input_state(IN_4) == TRUE && input_state(OUT_4) == FALSE) {
      delay_ms(50);
      if (input_state(IN_4) == TRUE && input_state(OUT_4) == FALSE) {
         output_high(OUT_4);
      }
      
   }
   else if (input_state(IN_4) == FALSE && input_state(OUT_4) == TRUE) {
      delay_ms(50);
      if (input_state(IN_4) == FALSE && input_state(OUT_4) == TRUE) {
         output_low(OUT_4);
      }
   }
   
   if (input_state(IN_5) == TRUE && input_state(OUT_5) == FALSE) {
      delay_ms(50);
      if (input_state(IN_5) == TRUE && input_state(OUT_5) == FALSE) {
          output_high(OUT_5);
      }
   }
   else if (input_state(IN_5) == FALSE && input_state(OUT_5) == TRUE) {
      delay_ms(50);
      if (input_state(IN_5) == FALSE && input_state(OUT_5) == TRUE) {
          output_low(OUT_5);
      }
   }
   
   if (input_state(IN_6) == TRUE && input_state(OUT_6) == FALSE) {
      delay_ms(50);
      if (input_state(IN_6) == TRUE && input_state(OUT_6) == FALSE) {
          output_high(OUT_6);
      }      
   }
   else if (input_state(IN_6) == FALSE && input_state(OUT_6) == TRUE) {
      delay_ms(50);
      if (input_state(IN_6) == FALSE && input_state(OUT_6) == TRUE) {
          output_low(OUT_6);
      }
   }
   
   if (input_state(IN_7) == TRUE && input_state(OUT_7) == FALSE) {
      delay_ms(50);
      if (input_state(IN_7) == TRUE && input_state(OUT_7) == FALSE) {
          output_high(OUT_7);
      }
   }
   else if (input_state(IN_7) == FALSE && input_state(OUT_7) == TRUE) {
      delay_ms(50);
      if (input_state(IN_7) == FALSE && input_state(OUT_7) == TRUE) {
          output_low(OUT_7);
      }
   }
   
   if (input_state(IN_8) == TRUE && input_state(OUT_8) == FALSE) {
      delay_ms(50);
      if (input_state(IN_8) == TRUE && input_state(OUT_8) == FALSE) {
           output_high(OUT_8);
      }
     
   }
   else if (input_state(IN_8) == FALSE && input_state(OUT_8) == TRUE) {
      delay_ms(50);
      if (input_state(IN_8) == FALSE && input_state(OUT_8) == TRUE) {
          output_low(OUT_8);
      }
   }   
}

void main() {
   setup_adc(ADC_OFF);
   setup_adc_ports(NO_ANALOGS); 
   
   IOsConfig();
   
   delay_ms(2000);
   
   while (1) {
      InputsScan();
   } 
}
