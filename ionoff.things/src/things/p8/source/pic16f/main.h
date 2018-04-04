#include <16F723.h>

#FUSES NOWDT                    //No Watch Dog Timer
#FUSES HS                       //High speed Osc (> 4mhz for PCM/PCH) (>10mhz for PCD)
#FUSES NOPUT                    //No Power Up Timer
#FUSES MCLR                     //Master Clear pin enabled
#FUSES NOPROTECT                //Code not protected from reading
#FUSES NOBROWNOUT               //No brownout reset
#FUSES BORV19                
#FUSES PLLEN                 
#FUSES NODEBUG                  //No Debug mode for ICD
#FUSES NOVCAP                
#use delay(clock=20000000)


//Input pins--------------------------------------------------------------------
#define  IN_1  PIN_B5 // PIC18F OUT1
#define  IN_2  PIN_B4 // PIC18F OUT2
#define  IN_3  PIN_B3 // PIC18F OUT3
#define  IN_4  PIN_B2 // PIC18F OUT4
#define  IN_5  PIN_B1 // PIC18F OUT5
#define  IN_6  PIN_B0 // PIC18F OUT6
#define  IN_7  PIN_C5 // PIC18F OUT7
#define  IN_8  PIN_C4 // PIC18F OUT8

//Output pins-------------------------------------------------------------------
#define  OUT_1  PIN_A0 // RELAY 1
#define  OUT_2  PIN_A1 // RELAY 2
#define  OUT_3  PIN_A2 // RELAY 3
#define  OUT_4  PIN_A3 // RELAY 4
#define  OUT_5  PIN_A4 // RELAY 5
#define  OUT_6  PIN_A5 // RELAY 6
#define  OUT_7  PIN_C0 // RELAY 7
#define  OUT_8  PIN_C1 // RELAY 8

//In/Out flag pins--------------------------------------------------------------
#define  IN_P18F_RUNNING  PIN_C3 
#define  OUT_IAM_RUNNING  PIN_C2 

