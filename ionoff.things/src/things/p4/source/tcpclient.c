/*********************************************************************
 *
 *  TCP Client Application to send IO status notification to IOnOff server
 *
 *********************************************************************
 * FileName:        tcpclient.c
 * Dependencies:    tcp.h, config.h
 * Processor:       PIC18, PIC24F, PIC24H, dsPIC30, dsPIC33F
 * Complier:        CCS 4.15
 * Company:         IOnOff Technology.
 * PreCondition:    Stack is initialized()
 *
 * Author           Date          Comment
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * SannTran          30/06/2016      Customize from Generic TCP Client
 *                            Thanks to Microchip Inc
 ********************************************************************/

#define __TCPCLIENT_C


/*********************************************************************/
// Global variables

WORD CenterPort;
NODE_INFO Server;
BOOLEAN IsInputChanged;
BOOLEAN IsEnc28J60Reset;
int8 EncTransactionCount;
BOOLEAN IsTCPClientBusy;

// Internal variables
#define DATA_RECEIVED_LEN 32
#define CONF_SET_SRV_IP 1
#define CONF_SET_IP 2
#define CONF_SET_MASK 3
#define CONF_SET_GATE 4
#define CONF_SET_MAC 5

#define CMD_NA 0
#define CMD_IO 1
#define CMD_CF 2
#define CMD_ER 3

int8 CmdReceived;
char DataReceived[DATA_RECEIVED_LEN];

/*********************************************************************/

TCP_SOCKET ClientSocket = INVALID_SOCKET;

void ClientSockPut(char c) {
   TCPPut(ClientSocket, c);
}

/*********************************************************************
 * Function:        void TCPClientTask(void)
 *
 * PreCondition:    Stack is initialized()
 *
 * Input:           None
 *
 * Output:          None
 *
 * Side Effects:    None
 *
 * Overview:        None
 *
 * Note:            None
 ********************************************************************/
void TCPClientInit() {
   CenterPort = 8118;
   EncTransactionCount = 0;
   IsTCPClientBusy = FALSE;
   IsEnc28J60Reset = TRUE;
   Server.IPAddr.v[0] = MY_SRV_IP_BYTES[0];
   Server.IPAddr.v[1] = MY_SRV_IP_BYTES[1];
   Server.IPAddr.v[2] = MY_SRV_IP_BYTES[2];
   Server.IPAddr.v[3] = MY_SRV_IP_BYTES[3];
}

void LCDPrintCmdUK() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "ER:BADCMD");
#endif
}

void OutSet(int8 pin, char val, int8 idx) {

   if (val == '0') {
      output_low(pin);      
      EEPWriteOutputState(idx);
   }
   else if (val == '1') {
      output_high(pin);
      EEPWriteOutputState(idx);
   }
   else if (val == '2') {
      output_high(pin);
      delay_ms(920);
      output_low(pin);
      EEPWriteOutputState(idx);
   }
#ifdef USE_PORTD_LCD
   if (val == '0') {
      printf(lcd_putc, "\fSet OUT_%d 0", idx);
   }
   else if (val == '1') {
      printf(lcd_putc, "\fSet OUT_%d 1", idx);
   }
   else if (val == '2') {
      printf(lcd_putc, "\fSet OUT_%d 2", idx);
   }
#endif
}

// Method to set server ip, ip, subnet mask, gateway
void ConfigSet(int8 att) {
   int8 idx[5], i, sep;
   sep = 0;
   idx[0] = 8; // 8 is index of start ip value

   for (i = idx[0]; i < DATA_RECEIVED_LEN; i++) {
      if (DataReceived[i] == '-') { // use '-' instead of '.'
         sep++;
         if (sep <= 3) {
            idx[sep] = i;
         }
      }
      if (DataReceived[i] == '}') {
         sep++;
         if (sep <= 4) {
            idx[sep] = i;
         }
      }
   }

   if  (sep != 4) {
      CmdReceived = CMD_ER;
      LCDPrintCmdUK();
      return;
   }

   int8 b1s, b2s, b3s, b4s;
   b1s = idx[1] - idx[0];
   b2s = idx[2] - idx[1] - 1;
   b3s = idx[3] - idx[2] - 1;
   b4s = idx[4] - idx[3] - 1;
   if (b1s > 3 || b1s == 0 || b2s > 3 || b2s == 0 || b3s > 3 || b3s == 0
         || b4s > 3 || b4s == 0) {
     CmdReceived = CMD_ER;
     LCDPrintCmdUK();
      return;
   }

   char b1[4];
   char b2[4];
   char b3[4];
   char b4[4];

   for (i = 0; i < b1s; i++) {
      b1[i] = DataReceived[i + idx[0]];
   }
   for (i = 0; i < b2s; i++) {
      b2[i] = DataReceived[i + idx[1] + 1];
   }
   for (i = 0; i < b3s; i++) {
      b3[i] = DataReceived[i + idx[2] + 1];
   }
   for (i = 0; i < b4s; i++) {
      b4[i] = DataReceived[i + idx[3] + 1];
   }

   if (att == CONF_SET_SRV_IP) {
      MyEEPRom.SrvIps[0] = atoi(b1);
      MyEEPRom.SrvIps[1] = atoi(b2);
      MyEEPRom.SrvIps[2] = atoi(b3);
      MyEEPRom.SrvIps[3] = atoi(b4);
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\n%u.%u.%u.%u", MyEEPRom.SrvIps[0], MyEEPRom.SrvIps[1], MyEEPRom.SrvIps[2], MyEEPRom.SrvIps[3]);
#endif
      EEPWriteSrvIp();
   }
   else if (att == CONF_SET_IP) {
      MyEEPRom.Ips[0] = atoi(b1);
      MyEEPRom.Ips[1] = atoi(b2);
      MyEEPRom.Ips[2] = atoi(b3);
      MyEEPRom.Ips[3] = atoi(b4);
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\n%u.%u.%u.%u", MyEEPRom.Ips[0], MyEEPRom.Ips[1], MyEEPRom.Ips[2], MyEEPRom.Ips[3]);
#endif
      EEPWriteIp();
   }
   else if (att == CONF_SET_MASK) {
      MyEEPRom.Masks[0] = atoi(b1);
      MyEEPRom.Masks[1] = atoi(b2);
      MyEEPRom.Masks[2] = atoi(b3);
      MyEEPRom.Masks[3] = atoi(b4);
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\n%u.%u.%u.%u", MyEEPRom.Masks[0], MyEEPRom.Masks[1], MyEEPRom.Masks[2], MyEEPRom.Masks[3]);
#endif
      EEPWriteMask();
   }
   else if (att == CONF_SET_GATE) {
      MyEEPRom.Gates[0] = atoi(b1);
      MyEEPRom.Gates[1] = atoi(b2);
      MyEEPRom.Gates[2] = atoi(b3);
      MyEEPRom.Gates[3] = atoi(b4);
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\n%u.%u.%u.%u", MyEEPRom.Gates[0], MyEEPRom.Gates[1], MyEEPRom.Gates[2], MyEEPRom.Gates[3]);
#endif
      EEPWriteGate();
   }

   else if (att == CONF_SET_MAC) {
      // continue parsing 2 last mac bytes
      // 4 bytes are the last 4 bytes of 6 bytes mac
      MyEEPRom.Macs[0] = atoi(b1);
      MyEEPRom.Macs[1] = atoi(b2);
      MyEEPRom.Macs[2] = atoi(b3);
      MyEEPRom.Macs[3] = atoi(b4);
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\n%X%X%X%X", MyEEPRom.Macs[0], MyEEPRom.Macs[1], MyEEPRom.Macs[2], MyEEPRom.Macs[3]);
#endif
      EEPWriteMac();
   }
}

void InTypeSet() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "Set InType");
   printf(lcd_putc, "\n%s", DataReceived);
#endif
   int i;
   for (i = 8; i < 16; i++) {
      if (DataReceived[i] == '1') {
         MyEEPRom.InputTypes[i - 8] = INPUT_SWITCH;
      }
      else if (DataReceived[i] == '2') {
         MyEEPRom.InputTypes[i - 8] = INPUT_BUTTON;
      }
   }
   EEPWriteInputTypes();
}

void SrvSet() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "Set SrvIP");
#endif
   ConfigSet(CONF_SET_SRV_IP);
}

void IpSet() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "Set MyIP");
#endif
   ConfigSet(CONF_SET_IP);
}

void MaskSet() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "Set SubnetMask");
#endif
   ConfigSet(CONF_SET_MASK);
}

void GateSet() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "Set GateWay");
#endif
   ConfigSet(CONF_SET_GATE);
}

void MacSet() {
#ifdef USE_PORTD_LCD
   printf(lcd_putc, "\f%s", "Set Mac");
#endif
   ConfigSet(CONF_SET_MAC);
}

BOOLEAN ObjFound(char c1, char c2) {
   return DataReceived[1] == c1 && DataReceived[2] == c2;
}

BOOLEAN CmdFound(char c1, char c2, char c3) {
   return DataReceived[3] == c1 && DataReceived[4] == c2 && DataReceived[5] == c3;
}

BOOLEAN AttFound(char c1, char c2) {
   return DataReceived[6] == c1 && DataReceived[7] == c2;
}

void IOReqHandle(void) {
   if (CmdFound('g', 'e', 't')) { // Check get io status
      // does nothing, forward to next step to return IO status
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "Get IO State");
#endif
   }
   else if (CmdFound('s', 'e', 't')) { // Check set io status
      char val = DataReceived[8];

      if (AttFound('o', '1')) {
         OutSet(OUT_1, val, 1);
      }
      else if (AttFound('o', '2')) {
         OutSet(OUT_2, val, 2);
      }
      else if (AttFound('o', '3')) {
         OutSet(OUT_3, val, 3);
      }
      else if (AttFound('o', '4')) {
         OutSet(OUT_4, val, 4);
      }
      else if (AttFound('o', '5')) {
         OutSet(OUT_5, val, 5);
      }
      else if (AttFound('o', '6')) {
         OutSet(OUT_6, val, 6);
      }
      else if (AttFound('o', '7')) {
         OutSet(OUT_7, val, 7);
      }
      else if (AttFound('o', '8')) {
         OutSet(OUT_8, val, 8);
      }
      else {
         CmdReceived = CMD_ER;
         LCDPrintCmdUK();
      }
   }
   else {
      CmdReceived = CMD_ER;
      LCDPrintCmdUK();
   }
}

void TCPLedOn() {
   output_low(OUT_TCP_STATE);
}

void TCPLedOff() {
   output_high(OUT_TCP_STATE);
}

void CfReqHandle(void) {
   if (CmdFound('g', 'e', 't')) { // Check to get configs
      // Go to next step to return configs
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "Get Config");
#endif
   }
   else if (CmdFound('s', 'e', 't')) { // Check to set configs
      if (AttFound('i', 'n')) { // Set input type switch / button
         InTypeSet();
      }
      else if (AttFound('s', 'v')) { // Set ionoff server ip
         SrvSet();         
      }
      else if (AttFound('i', 'p')) { // Set my board ip
         IpSet();
      }
      else if (AttFound('s', 'm')) { // Set subnet mask
         MaskSet();
      }
      else if (AttFound('g', 'w')) { // Set gate way
         GateSet();
      }
      else if (AttFound('m', 'a')) { // Set gate way
         MacSet();
      }
      else {
         CmdReceived = CMD_ER;
         LCDPrintCmdUK();
      }
   }
   else {
      CmdReceived = CMD_ER;
      LCDPrintCmdUK();
   }
}

void TCPClientTask(void) {
   
   char BuffC;
   int8 BuffI;
   static int8 ARPCheckResolved;
   static int8 TCPCheckConnected;
   static TICKTYPE Timer;   

   static enum {
      SM_HOME = 0,
      SM_ARP_START_RESOLVE = 1,
      SM_ARP_RESOLVE = 2,
      SM_SOCKET_OBTAIN = 3,
      SM_SOCKET_OBTAINED = 4,
      SM_PROCESS_RESPONSE = 5,
      SM_DISCONNECT = 6,
      SM_DONE = 7
   } TCPClientState = SM_HOME;
 
   switch (TCPClientState) {
   case SM_HOME:      
      ARPCheckResolved = 0;
      TCPCheckConnected = 0;
      IsTCPClientBusy = FALSE;
      EncTransactionCount = EncTransactionCount + 1;
      CmdReceived = CMD_NA;
      TCPClientState = SM_ARP_START_RESOLVE;
      break;

   case SM_ARP_START_RESOLVE:
      // Obtain the MAC address associated with the server's IP address
      // (either direct MAC address on same subnet, or the MAC address of the Gateway machine)
      ARPResolve(&Server.IPAddr);
      Timer = TickGet();
      TCPClientState = SM_ARP_RESOLVE;
      break;

   case SM_ARP_RESOLVE:
      
      // Wait for the MAC address to finish being obtained
      if (!ARPIsResolved(&Server.IPAddr, &Server.MACAddr)) {
         // Time out if too much time is spent in this state
         if (TickGet() - Timer > 3 * TICK_SECOND) {            
            if (ARPCheckResolved >= 3) {
#ifdef USE_PORTD_LCD
               printf(lcd_putc, "\f%s", "ARPNotResolved/");
#endif
               TCPClientState = SM_DONE; // ignore sending this notification
            }
            else {
#ifdef USE_PORTD_LCD
               printf(lcd_putc, "\f%s", "ARPNotResolved+");
#endif
               TCPClientState = SM_ARP_START_RESOLVE; // try to reconnect
            }
            ARPCheckResolved ++;
         }
         break;
      }
      Timer = TickGet();
      TCPClientState = SM_SOCKET_OBTAIN;
      break;

   case SM_SOCKET_OBTAIN:
      
      // Connect a socket to the remote TCP server
      ClientSocket = TCPConnect(&Server, CenterPort);
      // Abort operation if no TCP sockets are available
      // If this ever happens, incrementing MAX_TCP_SOCKETS in
      // stacktsk.h may help (at the expense of more global memory
      // resources).
      if (ClientSocket == INVALID_SOCKET) {
         if (TickGet() - Timer > 3 * TICK_SECOND) {
#ifdef USE_PORTD_LCD
            printf(lcd_putc, "\f%s", "TCPConnectFailed");
#endif  
            TCPClientState = SM_DONE;         
         }
         break;
      }
      TCPClientState = SM_SOCKET_OBTAINED;
      Timer = TickGet();
      break;

   case SM_SOCKET_OBTAINED:
      // Wait for the remote server to accept connection request
      if (!TCPIsConnected(ClientSocket)) {
         // Time out if too much time is spent in this state
         if (TickGet() - Timer > 3 * TICK_SECOND) {
            TCPClientState = SM_DISCONNECT;
            break;           
         }
         break;
      }
     TCPLedOn();     
     IsTCPClientBusy = TRUE;
      
      
#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\f%s", "TCPNowConnected.");
#endif
      // Make certain the socket can be written to
      if (!TCPIsPutReady(ClientSocket)) {
         break;
      }
      char ioStates[18];
      
      if (CmdReceived == CMD_NA || CmdReceived == CMD_IO) {
         // Place the data into the transmit buffer.
         sprintf(ioStates, "%d%d%d%d%d%d%d%d,%d%d%d%d%d%d%d%d",
               input_state(IN_1), input_state(IN_2), input_state(IN_3),
               input_state(IN_4), input_state(IN_5), input_state(IN_6),
               input_state(IN_7), input_state(IN_8),
               input_state(OUT_1),
               input_state(OUT_2), input_state(OUT_3), input_state(OUT_4),
               input_state(OUT_5), input_state(OUT_6), input_state(OUT_7),
               input_state(OUT_8));
         if (CmdReceived == CMD_NA) {
            if (IsEnc28J60Reset == TRUE){
               printf(ClientSockPut, "RS:%s,%X%X%X%X\n", ioStates, MY_MAC_BYTE3, MY_MAC_BYTE4, MY_MAC_BYTE5, MY_MAC_BYTE6);
            }      
            else if (IsInputChanged == TRUE){
               printf(ClientSockPut, "CH:%s,%X%X%X%X\n", ioStates, MY_MAC_BYTE3, MY_MAC_BYTE4, MY_MAC_BYTE5, MY_MAC_BYTE6);
            }
            else {
               printf(ClientSockPut, "ST:%s,%X%X%X%X\n", ioStates, MY_MAC_BYTE3, MY_MAC_BYTE4, MY_MAC_BYTE5, MY_MAC_BYTE6);
            }
            if (IsInputChanged == TRUE){
               IsInputChanged = FALSE;
            }
            if (IsEnc28J60Reset == TRUE){
               IsEnc28J60Reset = FALSE;
            }
         }
         
         else { // CmdReceived == CMD_IO 
            printf(ClientSockPut, "IO:%s\n", ioStates);
         }         
      }
      else if (CmdReceived == CMD_CF) {
         char inputTypes[9];
         sprintf(inputTypes, "%d%d%d%d%d%d%d%d",
               MyEEPRom.InputTypes[0], MyEEPRom.InputTypes[1], MyEEPRom.InputTypes[2], MyEEPRom.InputTypes[3],
               MyEEPRom.InputTypes[4], MyEEPRom.InputTypes[5], MyEEPRom.InputTypes[6], MyEEPRom.InputTypes[7]);
         printf(ClientSockPut,
            "CF:%X.%X.%X.%X,%X.%X.%X.%X,%X.%X.%X.%X,%X.%X.%X.%X,%X-%X-%X-%X,%s\n",
            MyEEPRom.SrvIps[0], MyEEPRom.SrvIps[1], MyEEPRom.SrvIps[2], MyEEPRom.SrvIps[3],
            MyEEPRom.Ips[0], MyEEPRom.Ips[1], MyEEPRom.Ips[2], MyEEPRom.Ips[3],
            MyEEPRom.Masks[0], MyEEPRom.Masks[1], MyEEPRom.Masks[2], MyEEPRom.Masks[3], 
            MyEEPRom.Gates[0], MyEEPRom.Gates[1], MyEEPRom.Gates[2], MyEEPRom.Gates[3],
            // Just send mac from byte 3 to 6
            MyEEPRom.Macs[0], MyEEPRom.Macs[1], MyEEPRom.Macs[2], MyEEPRom.Macs[3],
            inputTypes);
      }
      else { //CmdReceived == CMD_ER
         printf(ClientSockPut, "ER:BADCMD\n");
      }
      
      // Send the packet
      TCPFlush(ClientSocket);
      if (CmdReceived == CMD_NA) {
         TCPClientState = SM_PROCESS_RESPONSE;
      }
      else {
         TCPClientState = SM_DISCONNECT;
      }
      Timer = TickGet();
      break;

   case SM_PROCESS_RESPONSE:
      IsTCPClientBusy = TRUE;
      // Check to see if the remote node has disconnected from us or sent us any application data
      if (!TCPIsConnected(ClientSocket)) {
         IsTCPClientBusy = FALSE;
         TCPClientState = SM_DISCONNECT;
         break;
      }
      if (!TCPIsGetReady(ClientSocket)) {
         IsTCPClientBusy = FALSE;
         if (TickGet() - Timer > 30 * TICK_SECOND) {
            TCPClientState = SM_DISCONNECT;
         }
         if (IsInputChanged == TRUE) {
            TCPClientState = SM_DISCONNECT;
         }
         break;
      }
      TCPLedOn();
      BuffI = 0;
      // Obtain the server reply
      while (TCPGet(ClientSocket, &BuffC)) {
         DataReceived[BuffI] = BuffC;
         BuffI++;
         if (BuffC == '\n') {
            break;
         }
      }

#ifdef USE_PORTD_LCD
      printf(lcd_putc, "\fREQ:%s", DataReceived);
#endif
      // Handle Data Received
      if (ObjFound('i', 'o')) {
         IOReqHandle();
         CmdReceived = CMD_IO;
      }
      else if (ObjFound('c', 'f')) {
         CfReqHandle();
         CmdReceived = CMD_CF;
      }
      else {
         LCDPrintCmdUK();
         CmdReceived = CMD_ER;
      }

      // Continue reading data...
      Timer = TickGet();
      TCPClientState = SM_SOCKET_OBTAINED;
      break;

   case SM_DISCONNECT:
      // Close the socket so it can be used by other modules
      // For this application, we wish to stay connected, but this state will still get entered if the remote server decides to disconnect
      TCPDisconnect(ClientSocket);
      ClientSocket = INVALID_SOCKET;
      TCPClientState = SM_DONE;      
      break;

   case SM_DONE:
      TCPClientState = SM_HOME;   
      TCPLedOff();
      break;
   }
}
