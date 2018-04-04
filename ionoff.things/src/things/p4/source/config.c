/*********************************************************************
 * FileName:        config.c
 * Dependencies:    config.h
 * Processor:       PIC18, PIC24F, PIC24H, dsPIC30, dsPIC33F
 * Complier:        Microchip C18 v3.03 or higher
 *                  Microchip C30 v2.01 or higher
 * Company:         IOnOff Technology, Inc.
 *
 * Author               Date       Comment
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Sann Tran            30/06/16   Original
 ********************************************************************/
#define __CONFIG_C

void MacReset(void) {    
   // Microchip VendorID, MAC: 00-04-A3-XX-XX-XX
   MyEEPRom.Macs[0] = MY_DEFAULT_MAC_BYTE3;
   MyEEPRom.Macs[1] = MY_DEFAULT_MAC_BYTE4;
   MyEEPRom.Macs[2] = MY_DEFAULT_MAC_BYTE5;
   MyEEPRom.Macs[3] = MY_DEFAULT_MAC_BYTE6;
}

void ConfigReset(void) {
   MyEEPRom.Flag = TRUE;  
   
   MyEEPRom.SrvIps[0] = 192;
   MyEEPRom.SrvIps[1] = 168;
   MyEEPRom.SrvIps[2] = 1;
   MyEEPRom.SrvIps[3] = 252;

   MyEEPRom.Ips[0] = MY_DEFAULT_IP_ADDR_BYTE1;
   MyEEPRom.Ips[1] = MY_DEFAULT_IP_ADDR_BYTE2;
   MyEEPRom.Ips[2] = MY_DEFAULT_IP_ADDR_BYTE3;
   MyEEPRom.Ips[3] = MY_DEFAULT_IP_ADDR_BYTE4;

   //Mascara de Subred
   MyEEPRom.Masks[0] = MY_DEFAULT_MASK_BYTE1;
   MyEEPRom.Masks[1] = MY_DEFAULT_MASK_BYTE2;
   MyEEPRom.Masks[2] = MY_DEFAULT_MASK_BYTE3;
   MyEEPRom.Masks[3] = MY_DEFAULT_MASK_BYTE4;

   //Puerta de Enlace
   MyEEPRom.Gates[0] = MY_DEFAULT_GATE_BYTE1;
   MyEEPRom.Gates[1] = MY_DEFAULT_GATE_BYTE2;
   MyEEPRom.Gates[2] = MY_DEFAULT_GATE_BYTE3;
   MyEEPRom.Gates[3] = MY_DEFAULT_GATE_BYTE4; 

   // Set default input types
   int8 i;
   for (i = 0; i < 8; i++) {
      MyEEPRom.InputTypes[i] = INPUT_SWITCH;
   }
}

void EEPWriteFlag() {
   write_eeprom(0, MyEEPRom.Flag);
}

void EEPWriteSrvIp() {
   int i;
   for (i = 0; i < 4; i++) {
      write_eeprom(1 + i, MyEEPRom.SrvIps[i]);
   }   
}

void EEPWriteIp() {
   int i;
   for (i = 0; i < 4; i++) {
      write_eeprom(5 + i, MyEEPRom.Ips[i]);
   }
}

void EEPWriteGate() {
   int i;
   for (i = 0; i < 4; i++) {
      write_eeprom(9 + i, MyEEPRom.Gates[i]);
   }
}

void EEPWriteMask() {
   int i;
   for (i = 0; i < 4; i++) {
      write_eeprom(13 + i, MyEEPRom.Masks[i]);
   }
}

void EEPWriteMac() {  
   // Just store MAC bytes from 3-6
   int i;
   for (i = 0; i < 4; i++) {
      write_eeprom(17 + i, MyEEPRom.Macs[i]);
   }
}

void EEPWriteInputTypes() { // 21 - 28
   int8 i;
   for (i = 0; i < 8; i++) {
      write_eeprom(i + 21, MyEEPRom.InputTypes[i]);
   }
}

void EEPWriteOutputState(int8 idx) { // 29 - 36
   if (idx >= 1 && idx <= 8) {
      if (idx == 1) {
         write_eeprom(29, input_state(OUT_1));
      }
      else if (idx == 2) {
         write_eeprom(30, input_state(OUT_2));
      }
      else if (idx == 3) {
         write_eeprom(31, input_state(OUT_3));
      }
      else if (idx == 4) {
         write_eeprom(32, input_state(OUT_4));
      }
      else if (idx == 5) {
         write_eeprom(33, input_state(OUT_5));
      }
      else if (idx == 6) {
         write_eeprom(34, input_state(OUT_6));
      } 
      else if (idx == 7) {
         write_eeprom(35, input_state(OUT_7));
      }
      else if (idx == 8) {
         write_eeprom(36, input_state(OUT_8));
      }      
   }   
}

void EEPWriteConfig() {
   EEPWriteFlag();
   EEPWriteSrvIp();
   EEPWriteIp();
   EEPWriteGate();
   EEPWriteMask();
   EEPWriteInputTypes();
}

void EEPReadAll() {
   MyEEPRom.Flag = read_eeprom(0);
   int8 i;
   for (i = 0; i < 4; i++) {
      MyEEPRom.SrvIps[i] = read_eeprom(1 + i);
   }
   for (i = 0; i < 4; i++) {
      MyEEPRom.Ips[i] = read_eeprom(5 + i);
   }
   for (i = 0; i < 4; i++) {
      MyEEPRom.Gates[i] = read_eeprom(9 + i);
   }
   for (i = 0; i < 4; i++) {
      MyEEPRom.Masks[i] = read_eeprom(13 + i);
   }
   for (i = 0; i < 4; i++) {
      MyEEPRom.Macs[i] = read_eeprom(17 + i);
   }   
   for (i = 0; i < 8; i++) {
      MyEEPRom.InputTypes[i] = read_eeprom(21 + i);
   }
}

