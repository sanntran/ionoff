// Conexión entre el PIC18F4550 y el Modulo ENC28J60 (Se conecta directamente sin adaptador de voltajes).
// Se adjunta el datasheet del Modulo ENC28J60.

/*
// The connection to enc28j60 is now defined in enc28j60.c file

#define  PIN_ENC_MAC_SO    PIN_C7   // Connect from PIN MISO to ENC28J60.
#define  PIN_ENC_MAC_SI    PIN_C6   // Connect from PIN MOSI to ENC28J60.
#define  PIN_ENC_MAC_CLK   PIN_D1   // Connect from PIN SCK to ENC28J60.
#define  PIN_ENC_MAC_CS    PIN_C0   // Connect from PIN CS to ENC28J60.
#define  PIN_ENC_MAC_RST   PIN_C2   // Connect from PIN RST to ENC28J60.
#define  PIN_ENC_MAC_INT   PIN_B1   // Connect from PIN INT to ENC28J60.
#define  PIN_ENC_MAC_WOL   PIN_B3   // Connect from PIN WOL to ENC28J60.
#define  PIN_ENC_MAC_CS    PIN_D3   // Connect from PIN CS to ENC28J60.
#define  PIN_ENC_MAC_RST   PIN_D2   // Connect from PIN RST to ENC28J60.
*/    

//Protocolos a utilizar.                           -----------------------------

#define  STACK_USE_MCPENC  TRUE
#define  STACK_USE_ARP     TRUE
#define  STACK_USE_ICMP    TRUE
#define  STACK_USE_TCP     TRUE

//Comment to disable LCD
//#define USE_PORTD_LCD TRUE

//Output pins-------------------------------------------------------------------
#define  OUT_8  PIN_E2 // A4
#define  OUT_7  PIN_C0 // A5
#define  OUT_6  PIN_C1 // E0
#define  OUT_5  PIN_C2 // E1

#define  OUT_4  PIN_A4 // E2
#define  OUT_3  PIN_A5 // C0
#define  OUT_2  PIN_E0 // C1
#define  OUT_1  PIN_E1 // C2
#define  OUT_TCP_STATE PIN_B4
#define  OUT_IAM_RUNNING PIN_D0

//Input pins-------------------------------------------------------------------
#define  IN_8  PIN_D6 // B2
#define  IN_7  PIN_D5 // B1
#define  IN_6  PIN_C5 // B0
#define  IN_5  PIN_C4 // D7

#define  IN_4  PIN_B2 // D6
#define  IN_3  PIN_B1 // D5
#define  IN_2  PIN_B0 // C5
#define  IN_1  PIN_D7 // C4
#define  IN_RESET  PIN_B3
#define  IN_16F_RUNNING PIN_D1

#define INPUT_SWITCH 1
#define INPUT_BUTTON 2

BYTE MY_INPUT_TYPE_BYTES[8];
BYTE MY_SRV_IP_BYTES[4];

typedef struct _EEP_ROM {

   BYTE Flag;
   BYTE SrvIps[4];
   BYTE Ips[4];
   BYTE Masks[4];
   BYTE Gates[4];
   BYTE Macs[4]; // MAC bytes from 3-6
   BYTE InputTypes[8];   
} EEP_ROM;

EEP_ROM MyEEPRom;
