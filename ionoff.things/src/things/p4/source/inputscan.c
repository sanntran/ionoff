/*********************************************************************
 *
 * Inputs Changed Handler to handle input change. When input changed,
 * output pin will be switched state and trigger InChangedFlag to make
 * TCPClient send notification to IOnOff server
 *
 *********************************************************************
 * FileName:        inlisten.c
 * Dependencies:    tcp.h, dns.h, tcpclient.c, config.h
 * Processor:       PIC18, PIC24F, PIC24H, dsPIC30, dsPIC33F
 * Complier:        CCS 4.15
 * Company:         IOnOff Technology.
 *
 * Author           Date          Comment
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * SannTran          30/06/2016      Original
 ********************************************************************/
// 9 includes RESET button
BOOLEAN InStates[9];
BOOLEAN NewInStates[9];
BOOLEAN InStatesChanged[9];
TICKTYPE InStatesChangedTime[9];
BOOLEAN ControlDecisions[9];

//
TICKTYPE ResetPressedTime;

// Init pin as output and input
void InsStore(void) {
   InStates[0] = input_state(IN_RESET);
   InStates[1] = input_state(IN_1);
   InStates[2] = input_state(IN_2);
   InStates[3] = input_state(IN_3);
   InStates[4] = input_state(IN_4);
   InStates[5] = input_state(IN_5);
   InStates[6] = input_state(IN_6);
   InStates[7] = input_state(IN_7);
   InStates[8] = input_state(IN_8);
}

void InputsScanInit(void) {
   InsStore();
   TICKTYPE tick = TickGet();
   int8 i;
   for (i = 0; i < 9; i++) {      
      InStatesChanged[i] = FALSE;
      InStatesChangedTime[i] = tick;
   }
   IsInputChanged = FALSE;
   ResetPressedTime = 0;
}

void OnResetBtnReleased(void) {
   if ((TickGet() - ResetPressedTime) > 5 * TICK_SECOND) {
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "ResetAll...");
#endif
      ConfigReset();
      EEPWriteConfig();
   }
   else {
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "ResetPIC...");
#endif
   }
   // Just delay to let WDT reset
   output_low(OUT_IAM_RUNNING);
   delay_ms(10);
   setup_wdt(WDT_ON);
   restart_wdt();
}

void InChangedHandle(int8 pin, int8 idx) {
#ifdef USE_PORTD_LCD
    printf(lcd_putc, "\fIN_%d Changed OK", idx);
#endif

    IsInputChanged = TRUE;
    output_toggle(pin);
    EEPWriteOutputState(idx);
}

void InsListen(void) {   
   int8 i;
   for (i = 0; i < 9; i++) {
      ControlDecisions[i] = FALSE;
   }
   NewInStates[0] = input_state(IN_RESET);
   NewInStates[1] = input_state(IN_1);
   NewInStates[2] = input_state(IN_2);
   NewInStates[3] = input_state(IN_3);
   NewInStates[4] = input_state(IN_4);
   NewInStates[5] = input_state(IN_5);
   NewInStates[6] = input_state(IN_6);
   NewInStates[7] = input_state(IN_7);
   NewInStates[8] = input_state(IN_8);

   for (i = 0; i < 9; i++) {
      // If the input has changed before
      if (InStatesChanged[i] == TRUE) {
         
        // If the input has changed for more than ~1/3 second
        if ((TickGet() - InStatesChangedTime[i]) > 3) {
          
          // If the input state now is changed from stored input state
          if ((InStates[i] == FALSE && NewInStates[i] == TRUE) ||
                  (InStates[i] == TRUE && NewInStates[i] == FALSE)) {
             // The input state change is confirmed
             InStates[i] = NewInStates[i];
             
             
             if (i == 0) { // this is reset input button
                // Reset btn is pressed
                if (NewInStates[i] == FALSE) {
                   ResetPressedTime = TickGet();
#ifdef USE_PORTD_LCD
                   printf(lcd_putc, "\f%s", "Reset-Pressed");
#endif
                }                
                else { // Reset btn is released
                   OnResetBtnReleased();
                }                
             }
             else { // these are normal inputs for controling relays
               if (MY_INPUT_TYPE_BYTES[i - 1] == INPUT_BUTTON) {
                  if (NewInStates[i] == FALSE) { // released button
                     ControlDecisions[i] = TRUE;
                  }
               }
               else {
                  ControlDecisions[i] = TRUE;
               }
             }
             
             InStatesChanged[i] = FALSE; 
          }
          else {
             InStatesChanged[i] = FALSE;
          }
        }
      }
      // If new input state is changed from stored input state
      else if ((InStates[i] == FALSE && NewInStates[i] == TRUE) ||
         (InStates[i] == TRUE && NewInStates[i] == FALSE)) {
         #ifdef USE_PORTD_LCD
            printf(lcd_putc, "\fIN_%d%d%d", i + 1, InStates[i], NewInStates[i]);
         #endif
         InStatesChanged[i] = TRUE;
         InStatesChangedTime[i] = TickGet();
      }
   }

   if (ControlDecisions[1] == TRUE) {
      InChangedHandle(OUT_1, 1);
   }
   if (ControlDecisions[2] == TRUE) {
      InChangedHandle(OUT_2, 2);
   }      
   if (ControlDecisions[3] == TRUE) {
      InChangedHandle(OUT_3, 3);
   }
   if (ControlDecisions[4] == TRUE) {
      InChangedHandle(OUT_4, 4);
   }
   /*
   if (ControlDecisions[5] == TRUE) {
      InChangedHandle(OUT_5, 5);
   }
   if (ControlDecisions[6] == TRUE) {
      InChangedHandle(OUT_6, 6);
   }
   if (ControlDecisions[7] == TRUE) {
      InChangedHandle(OUT_7, 7);
   }
   if (ControlDecisions[8] == TRUE) {
      InChangedHandle(OUT_8, 8);
   }
   */
}
