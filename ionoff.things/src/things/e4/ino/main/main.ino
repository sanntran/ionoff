#include <stdarg.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <PubSubClient.h>

#define ESP_LED  2

#define OUT_1 13
#define OUT_2 12
#define OUT_3 14


#define IN_1  0
#define IN_2  4 
#define IN_3  5

#define MQTT_USER "ionoff"
#define MQTT_PASS "I0n0FFNet"
#define MQTT_TOPIC "IOnOffNet"

const byte MODE_AP = 1;
const byte MODE_STA = 2;

// Address 0 esp state
// Address 1 - 16   : is serial number
// Address 17 - 20  : input type
// Address 21       : wifi id length
// Address 22 - 52  : wifi id
// Address 53       : wifi password length
// Address 54 - 84  : wifi password
// Address 85       : tcp server host length
// Address 86 - 116 : tcp server host
// Address 117 - 120: output status

const byte SWITCH = 1;
const byte BUTTON = 2;
const byte SENSOR = 3;
const boolean USE_SERIAL = true;

const int TIMER_T = 20;

const int ST_RESET = 1;
const int ST_STATUS = 2;
const int ST_CHANGED = 3;

const int ID_LENGTH = 17;
const int HOST_LENGTH = 62;

const byte ESP_READY = 5;

const byte WIFI_CONNECTING = 6;
const byte SERVER_CONNECTING = 7;
const byte SERVER_CONNECTED = 8;

const int TCP_PORT = 1883;
const char TCP_SERVER[HOST_LENGTH] = "192.168.1.252";
const char SERIAL_ID[ID_LENGTH] = "E313180610";

const int WIFI_LENGTH = 31;
const char WIFI_ID[WIFI_LENGTH] = "IOnOffNet";
const char WIFI_PASS[WIFI_LENGTH] = "I0n0ffNet";

int stType;
byte espMode;
byte connState;

boolean firstMsg;
byte inputTypes[3];
char tcpServer[HOST_LENGTH];

char wifiId[WIFI_LENGTH];
char wifiPass[WIFI_LENGTH];
char serialStr[ID_LENGTH];

char ioStatus[8];  // 111,111
char stMessage[40]; // id=E310180101000000&code=status&io=111,111
                    // id=E310180101000000&code=hello&io=111,111
                    // id=E310180101000000&code=changed&io=111,111
                    // id=E310180101000000&code=reset&io=111,111
char ioMessage[42]; // id=E310180101000000&code=respio&io=111,111
char cfMessage[255]; // id=E310180101000000&code=respcf&cf=E310180101000000,mqtt.ionoff.net,221,IOnOffNet,I0n0ffNet

long outputLocks[] = {0, 0, 0};
byte inputStates[] = {HIGH, HIGH, HIGH};
byte newInputStates[] = {HIGH, HIGH, HIGH};
boolean inputChanges[] = {false, false, false};
long inputChangesTime[] = {0, 0, 0};
boolean controlDecisions[] = {false, false, false};
unsigned long espModeLedSignal = 0;

int wifiConn;

WiFiServer wifiServer(80);
WiFiClient wifiClient;
PubSubClient pubSubClient(wifiClient);

unsigned long lastPublishTime = 0;

extern "C" {
#include "user_interface.h"
}

os_timer_t timer;

void checkReleaseLocks() {
  for (int i = 0; i < 3; i++) {
    if (outputLocks[i] > 0) {
      outputLocks[i] = outputLocks[i] - TIMER_T;
      if (outputLocks[i] < 0) {
        outputLocks[i] = 0;
      }
      if (outputLocks[i] == 0) {
        if (i == 0) {
          digitalWrite(OUT_1, !digitalRead(OUT_1));
        }
        else if (i == 1) {
          digitalWrite(OUT_2, !digitalRead(OUT_2));
        }
        else if (i == 2) {
          digitalWrite(OUT_3, !digitalRead(OUT_3));
        }
        writeOutputStateToEepRom();
      }
    }
  }
}

void timerCallback(void *pArg) {
  checkInputChange();
  checkReleaseLocks();
  
  if (espMode == MODE_AP) {    
     if (espModeLedSignal >= 100) {
        espModeLedSignal = 0;
        digitalWrite(ESP_LED, LOW);
     }
     else if (espModeLedSignal % 10 == 0 && espModeLedSignal <= 50) {
        digitalWrite(ESP_LED, !digitalRead(ESP_LED));
     }
     espModeLedSignal = espModeLedSignal + 1;
  }
  else { // mode station
    if (espModeLedSignal >= 300) {
        espModeLedSignal = 0;
    }    
    if (connState == WIFI_CONNECTING) {
       if (espModeLedSignal % 25 == 0) {
          digitalWrite(ESP_LED, !digitalRead(ESP_LED));
       }
       espModeLedSignal = espModeLedSignal + 1;
    }
    else if (connState == SERVER_CONNECTING){
       if (espModeLedSignal % 25 == 0) {
         if (espModeLedSignal <= 150) {
           digitalWrite(ESP_LED, !digitalRead(ESP_LED));
         }
         else {
           digitalWrite(ESP_LED, LOW);
         }
       }
       espModeLedSignal = espModeLedSignal + 1;
    }
    // else: connState = SERVER_CONNECTED
  } 
}

void user_init(void) {
  os_timer_setfn(&timer, timerCallback, NULL);
  os_timer_arm(&timer, TIMER_T, true);
} // End of user_init

void setup() {
  delay(5);
  pinMode(IN_1, INPUT);
  pinMode(IN_2, INPUT);  
  pinMode(IN_3, INPUT);
    
  pinMode(OUT_1, OUTPUT);
  pinMode(OUT_2, OUTPUT);  
  pinMode(OUT_3, OUTPUT);
  pinMode(ESP_LED, OUTPUT); 
  
  digitalWrite(OUT_1, HIGH);
  digitalWrite(OUT_2, HIGH);
  digitalWrite(OUT_3, HIGH);    
  
  delay(250);  
 
  Serial.begin(9600);  
  EEPROM.begin(512); 
   
  delay(250);
  espMode = MODE_STA;

  // read adc value to set mode
  for (int i = 0; i < 3; i++) {
    boolean in2 = digitalRead(IN_2);
    if (in2 == false) {
      espMode = MODE_AP;
    }    
    delay(250);
    digitalWrite(ESP_LED, LOW);
    delay(250);
    digitalWrite(ESP_LED, HIGH);
  }   
  
  boolean in2 = digitalRead(IN_2);
  if (in2 == false && espMode == MODE_AP) {
    // Mode AP is confirmed
    espMode = MODE_AP;
  }
  else {
    espMode = MODE_STA;
  }
  
  delay(250);
  firstMsg = true;  
  loadSnFromEEPRom();
  if (checkSerialStr() == false) {
    delay(200);
    loadSnFromEEPRom();
    if (checkSerialStr() == false) {
      delay(200);
      loadSnFromEEPRom();
      if (checkSerialStr() == false) {
        if (USE_SERIAL) {
          Serial.println("Write default EEP ROM...");
        }
        resetSerial();
        resetConfig();
        writeSerialToEepRom();
        writeConfigToEepRom(); 
        writeOutputStateToEepRom();
        delay(200);  
        loadSnFromEEPRom();
      }
    }
  }
  delay(200);  
  loadStateFromEepRom();
  
  stType = ST_STATUS; 
  connState = WIFI_CONNECTING;
  
  user_init();
  
  WiFi.persistent(false);
  WiFi.softAPdisconnect();
  WiFi.disconnect();
  delay(250);
  if (espMode == MODE_STA) {    
    if (USE_SERIAL) {
      Serial.println("Start wifi in station mode");
    }        
    WiFi.mode(WIFI_STA);
    WiFi.setAutoConnect(false);  
    delay(100);
    wifiConn = 100;    
    connectWifi();
    pubSubClient.setServer(tcpServer, TCP_PORT);
    pubSubClient.setCallback(mqttCallback);    
  } 
  else if (espMode == MODE_AP) {
    if (USE_SERIAL) {
      Serial.println("Start wifi in acces point mode");
    }
    WiFi.mode(WIFI_AP);
    WiFi.softAP(serialStr);
    delay(250);
    wifiServer.begin();
  }  
}

boolean checkSerialStr() {
  for (int i = 0; i < 10; i++) {
    if (serialStr[i] != SERIAL_ID[i]) {
      return false;
    }
  }
  return true;
}

void connectWifi() {
  connState = WIFI_CONNECTING;
  if (wifiConn >= 100) {
    wifiConn = 0;
    if (USE_SERIAL) {
      Serial.println("Start connecting to wifi");
      Serial.print("Wifi id: ");
      Serial.println(wifiId);
      Serial.print("Wifi pw: ");
      Serial.println(wifiPass);
    }
    // FIX FOR USING 2.3.0 CORE (only .begin if not connected)
    if (WiFi.status() != WL_CONNECTED) {
      // connect to the network
      WiFi.begin(wifiId, wifiPass);
    }
  }
  else {
    delay(100);
    int wifiStat = WiFi.status();
    if (wifiStat == WL_CONNECTED ) {
      wifiConn = 100;
      if (USE_SERIAL) {
        Serial.println("Connected to wifi");
        Serial.print("WiFi.localIP: ");
        Serial.println(WiFi.localIP());
        Serial.print("WiFi.gatewayIP: ");
        Serial.println(WiFi.gatewayIP());
        Serial.print("WiFi.subnetMask: ");
        Serial.println(WiFi.subnetMask());
        Serial.print("WiFi.SSID: ");
        Serial.println(WiFi.SSID());
        Serial.print("WiFi.psk: ");
        Serial.println(WiFi.psk());
      }
      return;
    }
    else {
      wifiConn ++;
      if (wifiConn == 100) {
        stType = ST_STATUS;
        if (USE_SERIAL) {
          Serial.println("Cannot connect to wifi");
        }
        return;
      }
    }
  }
}

void mqttCallback(char* topic, byte* payload, unsigned int length) {
  String message = "";
  for (int i = 0; i < length; i++) {
    message += (char)payload[i];
  }   
  if (USE_SERIAL) {
      Serial.print("Message arrived: ");
      Serial.print(topic);
      Serial.print("/");
      Serial.println(message);
   }
   handleMessage(message);
}

void handleMessage(String cmd) {
  if (cmd.indexOf("{io") == 0) {
    handleIOCmd(cmd);
    sprintf(ioStatus, "%d%d%d,%d%d%d", inputStates[0], inputStates[1], inputStates[2],
            digitalRead(OUT_1), digitalRead(OUT_2), digitalRead(OUT_3));
    sprintf(stMessage, "id=%s&code=okcmd&io=%s", serialStr, ioStatus);
    if (USE_SERIAL) {
      Serial.print("Publish message: ");
      Serial.print(MQTT_TOPIC);
      Serial.print("/");
      Serial.println(stMessage);
    }    
    pubSubClient.publish(MQTT_TOPIC, stMessage);
  }
  else if (cmd.indexOf("{cf") == 0) {
    handleCFCmd(cmd); 
    sprintf(cfMessage, "id=%s&code=okcmd&cf=%s,%d%d%d,%s,%s,%s", 
      serialStr, serialStr, inputTypes[0], inputTypes[1], inputTypes[2], 
      tcpServer, wifiId, wifiPass);
    if (USE_SERIAL) {
      Serial.print("Publish message: ");
      Serial.print(MQTT_TOPIC);
      Serial.print("/");
      Serial.println(cfMessage);
    } 
    pubSubClient.publish(MQTT_TOPIC, cfMessage);
    if (cmd.indexOf("{cfsetmd") == 0) { // reset
      delay(1000);
      ESP.restart();
    }
  }
  else {
    char badCmd[95];
    sprintf(badCmd, "id=%s&code=badcmd", serialStr);
    if (USE_SERIAL) {
      Serial.print("Publish message: ");
      Serial.print(MQTT_TOPIC);
      Serial.print("/");
      Serial.println(badCmd);
    }
    pubSubClient.publish(MQTT_TOPIC, badCmd);
  }
}

void handleIOCmd(String cmd) {
  int t = 0;
  if (cmd.indexOf('}') > 9) {
    String s = cmd.substring(9, cmd.indexOf('}'));
    t = atoi(s.c_str());
  }  
  if (cmd.indexOf("{ioseto1") == 0) {
    setOutputState(OUT_1, cmd.charAt(8), t);
  }
  else if (cmd.indexOf("{ioseto2") == 0) {
    setOutputState(OUT_2, cmd.charAt(8), t);
  }
  else if (cmd.indexOf("{ioseto3") == 0) {
    setOutputState(OUT_3, cmd.charAt(8), t);
  }
  else {
    // does nothing
  }
}

void handleCFCmd(String cmd) {
  if (cmd.indexOf("{cfsetin") == 0) {
    for (int i = 0; i < 3; i++) {
      if (cmd.charAt(8 + i) == '1') {
        inputTypes[i] = SWITCH;
      }
      else if (cmd.charAt(8 + i) == '2') {
        inputTypes[i] = BUTTON;
      }
    }
    writeInputTypesToEepRom();
  }
  else if (cmd.indexOf("{cfsetsn") == 0) {
    String id = cmd.substring(8, cmd.indexOf('}'));
    sprintf(serialStr, "%s", id.c_str());
    writeSerialToEepRom();
    if (USE_SERIAL) {
      Serial.print("New serial: ");
      Serial.println(serialStr);
    }    
    writeSerialToEepRom();
  }
  else if (cmd.indexOf("{cfsetsv") == 0) {
    String srv = cmd.substring(8, cmd.indexOf('}'));
    sprintf(tcpServer, "%s", srv.c_str());
    writeSerialToEepRom();
    if (USE_SERIAL) {
      Serial.print("New server: ");
      Serial.println(tcpServer);
    }    
    writeServerIpToEepRom();    
  }
  else if (cmd.indexOf("{cfsetwi") == 0) {
    String wi = cmd.substring(8, cmd.indexOf('}'));
    sprintf(wifiId, "%s", wi.c_str());
    if (USE_SERIAL) {
      Serial.print("New wifi ID: ");
      Serial.println(wifiId);
    }
    writeWifiIdToEepRom();
  }
  else if (cmd.indexOf("{cfsetwp") == 0) {
    String wp = cmd.substring(8, cmd.indexOf('}'));
    sprintf(wifiPass, "%s", wp.c_str());
    if (USE_SERIAL) {
      Serial.print("New wifi password: ");
      Serial.println(wifiPass);
    }
    writeWifiPwToEepRom();
  }
  else if (cmd.indexOf("{cfsetmd") == 0) { // reset
    // Does nothing
    if (USE_SERIAL) {
      Serial.println("New mode * (RESET)");
    }
  }
}

void writeOutputStateToEepRom() {
  EEPROM.write(117, digitalRead(OUT_1));
  EEPROM.write(118, digitalRead(OUT_2));
  EEPROM.write(119, digitalRead(OUT_3));
  EEPROM.commit();
}

void writeSerialToEepRom() {
  // Serial NO is from 1 to 16
  for (int i = 0; i < 16; i++) {
    EEPROM.write(i + 1, serialStr[i]);
  }
  EEPROM.commit();
}

void writeInputTypesToEepRom() {
  // Wifi ID is from 17 - 20
  for (int i = 0; i < 3; i++) {
    EEPROM.write(i + 17, inputTypes[i]);
  }
  // next address (21) is of wifi id
  EEPROM.commit();
}

void writeWifiIdToEepRom() {
  // wifi id address is from 1 to 16
  // address 1 is length of wifi id
  byte wiLen = strlen(wifiId);
  EEPROM.write(21, wiLen);
  for (int i = 0; i < wiLen; i++) {
    EEPROM.write(i + 22, wifiId[i]);
  }
  EEPROM.commit();
}

void writeWifiPwToEepRom() {
  // wifi pw address is from 53 to 83
  // address 32 is length of wifi pw
  byte wpLen = strlen(wifiPass);
  EEPROM.write(53, wpLen);
  for (int i = 0; i < wpLen; i++) {
    EEPROM.write(i + 54, wifiPass[i]);
  }
  EEPROM.commit();
}

void writeServerIpToEepRom() {
  // Server Host is from 84 - 116
  byte srvLen = strlen(tcpServer);
   EEPROM.write(85, srvLen);
  for (int i = 0; i < srvLen; i++) {
    EEPROM.write(i + 86, tcpServer[i]);
  }
  EEPROM.commit();
}

void updateBtnStates() {
  newInputStates[0] = digitalRead(IN_1);
  newInputStates[1] = digitalRead(IN_2);
  newInputStates[2] = digitalRead(IN_3);  
}

void checkInputChange() {
  updateBtnStates();

  for (int i = 0; i < 3; i++) {
    controlDecisions[i] = false;
  }

  for (int i = 0; i < 3; i++) {

    if (inputChanges[i] == true) {
      if ((millis() - inputChangesTime[i]) >= 110) {
        if (newInputStates[i] != inputStates[i]) {
          inputStates[i] = newInputStates[i];
          // Accept input change
          if (inputTypes[i] == BUTTON) {
            if (newInputStates[i] == LOW) { // pressed button
              stType = ST_CHANGED;
              controlDecisions[i] = true;
              if (USE_SERIAL) {
                Serial.print("Button has been pressed ");
                Serial.println(i);
              }
            }
          }
          else {
            if (USE_SERIAL) {
              if (newInputStates[i] == LOW) {
                Serial.print("Switch has been closed ");
                Serial.println(i);
              }
              else {
                Serial.print("Switch has been open ");
                Serial.println(i);
              }
            }
            stType = ST_CHANGED;
            controlDecisions[i] = true;
          }          
          // Reset input change
          inputChanges[i] = false;
        }
        else {
          inputChanges[i] = false;
        }
      }
    }
    else if (newInputStates[i] != inputStates[i]) {
      inputChanges[i] = true;
      inputChangesTime[i] = millis();
    }
  }
  
  if (controlDecisions[0] == true) {
    if (inputTypes[0] == SENSOR) {
      setOutputState(OUT_1, newInputStates[0], 0);
    }
    else {
      changeOutputState(OUT_1);
    }
  }
  if (controlDecisions[1] == true) {
    if (inputTypes[1] == SENSOR) {
      setOutputState(OUT_2, newInputStates[1], 0);
    }
    else {
      changeOutputState(OUT_2);
    }
  }
  if (controlDecisions[2] == true) {
    if (inputTypes[2] == SENSOR) {
      setOutputState(OUT_3, newInputStates[2], 0);
    }
    else {
      changeOutputState(OUT_3);
    }
  }
}

void resetSerial() {
  byte mac[6];
  WiFi.macAddress(mac);
  sprintf(serialStr, "%s%02X%02X%02X", SERIAL_ID, mac[3], mac[4], mac[5]);
}

void resetConfig() {  
  for (int i = 0; i < 3; i++) {
    inputTypes[i] = BUTTON;
  }
  sprintf(wifiId, "%s", WIFI_ID);
  sprintf(wifiPass, "%s", WIFI_PASS);
  sprintf(tcpServer, "%s", TCP_SERVER);  
}

void loadSnFromEEPRom() {
  if (USE_SERIAL) {
    Serial.println("Load serial from EEPROM...");
  }  
  String idStr = "";
  char c;
  int idLen = 16;
  for (int i = 0; i < idLen; i++) {
    c = EEPROM.read(i + 1);
    idStr.concat(c);
  }
  sprintf(serialStr, "%s", idStr.c_str());  
  if (USE_SERIAL) {
    Serial.print("Serial no: ");
    Serial.println(serialStr);
  }
}

void loadStateFromEepRom() {
  if (USE_SERIAL) {
    Serial.println("Load state from EEPROM...");
  }
  String wiStr = "";
  String wpStr = "";  
  String srvStr = "";
  
  char c;
  
  for (int i = 0; i < 3; i++) {    
    inputTypes[i] = EEPROM.read(i + 17);
  }

  int wiLen = EEPROM.read(21);
  for (int i = 0; i < wiLen; i++) {
    c = EEPROM.read(i + 22);
    wiStr.concat(c);
  }
  
  int wpLen = EEPROM.read(53);
  for (int i = 0; i < wpLen; i++) {
    c = EEPROM.read(i + 54);
    wpStr.concat(c);
  } 
     
  int srvLen = EEPROM.read(85);
  for (int i = 0; i < srvLen; i++) {
    c = EEPROM.read(i + 86);
    srvStr.concat(c);
  }
  
  sprintf(wifiId, "%s",  wiStr.c_str());
  sprintf(wifiPass, "%s",  wpStr.c_str());
  sprintf(tcpServer, "%s",  srvStr.c_str());

  if (USE_SERIAL) {    
    Serial.print("Wifi id: ");
    Serial.println(wifiId);

    Serial.print("Wifi pw: ");
    Serial.println(wifiPass);

    Serial.print("Server ip: ");
    Serial.println(tcpServer);
  }

  // Read output status from byte index 117
  byte outStatus[3];
  int i = 0;
  for (i = 0; i < 3; i++) {
     outStatus[i] = EEPROM.read(117 + i);
  }

  if (USE_SERIAL) {
    Serial.println("Restore status from EEPRom...");
  }
  if (outStatus[0] == LOW) {
     delay(2500);
     digitalWrite(OUT_1, LOW);
  }
  if (outStatus[1] == LOW) {
     delay(2500);
     digitalWrite(OUT_2, LOW);
  }
  if (outStatus[2] == LOW) {
     delay(2500);
     digitalWrite(OUT_3, LOW);
  }
}

void writeConfigToEepRom() {
  writeInputTypesToEepRom();
  writeWifiIdToEepRom();
  writeWifiPwToEepRom();
  writeServerIpToEepRom();
}

void setOutputState(int outputPin, char state, int second) {
  int idx = 0;
  if (OUT_1 == outputPin) {
    idx = 0;
  }
  else if (OUT_2 == outputPin) {
    idx = 1;
  }
  else if (OUT_3 == outputPin) {
    idx = 2;
  }
  if (idx >= 3 || outputLocks[idx] != 0) {
    // does nothing
  }
  else {
    byte value = HIGH;
    if (state == '0') {
      value = LOW;
    }
    else if (state == '1') {
      value = HIGH;
    }
    else if (state == '2') {
      value = !digitalRead(outputPin);
    }
    digitalWrite(outputPin, value);
    outputLocks[idx] = second * 1000;
    writeOutputStateToEepRom();
  }  
}

void changeOutputState(int outputPin) {
  byte value = !digitalRead(outputPin);
  digitalWrite(outputPin, value);
  writeOutputStateToEepRom();
}

void apModeLoop() {

  // Check if a client has connected
  WiFiClient client = wifiServer.available();
  if (!client) {
    return;
  }
  // Read the first line of the request
  String req = client.readStringUntil('\r');

  if (USE_SERIAL) {   
    Serial.println(req);
  }
  client.flush();
  String cmd = "";
  int count = 0;
  for (int i = 0; i < req.length(); i++) {    
    if (req.charAt(i) == '/') {      
      count ++;
      if (count == 1) {
        cmd += "{";
      }
      else if (count == 2) {
         cmd += "}";
      }      
    }
    else if (count == 1) {
      cmd += req.charAt(i);
    }
  }

  String resp = "HTTP/1.1 200 OK\r\n";
  resp += "Content-Type: text/plain\r\n\r\n";
  
  if (cmd.indexOf("{io") == 0) {
     handleIOCmd(cmd);
     sprintf(ioStatus, "%d%d%d,%d%d%d", inputStates[0], inputStates[1], inputStates[2],
              digitalRead(OUT_1), digitalRead(OUT_2), digitalRead(OUT_3));
     sprintf(stMessage, "IO:%s", ioStatus);
     resp += stMessage; 
     resp += "\n";
  }
  else if (cmd.indexOf("{cf") == 0) {
     handleCFCmd(cmd); 
     sprintf(cfMessage, "CF:%s,%d%d%d,%s,%s,%s", 
          serialStr, inputTypes[0], inputTypes[1], inputTypes[2], 
          tcpServer, wifiId, wifiPass);
     resp += cfMessage;
     resp += "\n";
  }
  else {
      resp += "code=badcmd\n";
  }
  // Send the response to the client
  client.print(resp);
  client.flush();
  delay(10);
  if (USE_SERIAL) {
    Serial.println("Client disconnected");
  }
  if (cmd.indexOf("{cfsetmd") == 0) {
    delay(1000);
    ESP.restart();
  }
}

void staModeLoop() {
  if (WiFi.status() != WL_CONNECTED) {
      connectWifi();
  }
  else {
    
    if (!pubSubClient.connected()) {
      if (USE_SERIAL) {
        Serial.print("Connecting to MQTT server ");
        Serial.println(tcpServer);
      }
      connState = SERVER_CONNECTING;
      
      if (pubSubClient.connect(serialStr, MQTT_USER, MQTT_PASS)) {
        if (USE_SERIAL) {
          Serial.println("Connected to MQTT server");
        }
        connState = SERVER_CONNECTED;
        // Turn on ESP MODE LED SIGNAL
        digitalWrite(ESP_LED, LOW);
        
        sprintf(ioStatus, "%d%d%d,%d%d%d", inputStates[0], inputStates[1], inputStates[2],
              digitalRead(OUT_1), digitalRead(OUT_2), digitalRead(OUT_3));
        if (firstMsg == true) {
            sprintf(stMessage, "id=%s&code=reset&io=%s", serialStr, ioStatus);
        }
        else {
            sprintf(stMessage, "id=%s&code=hello&io=%s", serialStr, ioStatus);
        }        
        if (USE_SERIAL) {
          Serial.print("Publish message: ");
          Serial.print(MQTT_TOPIC);
          Serial.print("/");
          Serial.println(stMessage);
        }  
        pubSubClient.publish(MQTT_TOPIC, stMessage);        
        pubSubClient.subscribe(serialStr); 
        if (USE_SERIAL) {
          Serial.print("Subscribe topic: ");
          Serial.println(serialStr);
        }
        firstMsg = false;     
      }   
      else {
        if (USE_SERIAL) {
          Serial.println("Connect to MQTT server failed");
        }
        delay(5000);
      }
    }
    else {
        pubSubClient.loop();
        sprintf(ioStatus, "%d%d%d,%d%d%d", inputStates[0], inputStates[1], inputStates[2],
              digitalRead(OUT_1), digitalRead(OUT_2), digitalRead(OUT_3));
          
        if (stType == ST_CHANGED) {
          sprintf(stMessage, "id=%s&code=changed&io=%s", serialStr, ioStatus);     
          pubSubClient.publish(MQTT_TOPIC, stMessage);
          if (USE_SERIAL) {
            Serial.print("Publish message: ");
            Serial.print(MQTT_TOPIC);
            Serial.print("/");
            Serial.println(stMessage);
          }  
          stType = ST_STATUS;
          lastPublishTime = millis();      
        } 
        else if (millis() - lastPublishTime > 45000) {
          sprintf(stMessage, "id=%s&code=status&io=%s", serialStr, ioStatus);
          if (USE_SERIAL) {
            Serial.print("Publish message: ");
            Serial.print(MQTT_TOPIC);
            Serial.print("/");
            Serial.println(stMessage);
          }  
          pubSubClient.publish(MQTT_TOPIC, stMessage);
          stType = ST_STATUS; 
          lastPublishTime = millis();
        }   
    }
  }
}


void loop() {
  if (espMode == MODE_AP) {    
    apModeLoop();
  }
  else if (espMode == MODE_STA) {
    staModeLoop();
  }  
}

