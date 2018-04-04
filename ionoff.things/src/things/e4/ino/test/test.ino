#include <ESP8266WiFi.h>
#include <EEPROM.h>
#include <stdarg.h>

#define IN_ADC   A0
#define OUT_1    12
#define OUT_2    14
#define OUT_3    4
#define OUT_4    5
#define LED_ESP  2
#define LED_TCP  13

const boolean USE_SERIAL = true;

void setup() {
  pinMode(IN_ADC, INPUT);
  pinMode(OUT_1, OUTPUT);
  pinMode(OUT_2, OUTPUT);
  pinMode(OUT_3, OUTPUT);
  pinMode(OUT_4, OUTPUT);
  pinMode(LED_ESP, OUTPUT);
  pinMode(LED_TCP, OUTPUT);
  delay(500);
  if (USE_SERIAL) {
    Serial.begin(9600);
  }
  
}

void loop() {
  delay(1000);
  unsigned int adcValue = analogRead(IN_ADC);
  Serial.printf("ADC = %d\r\n", adcValue);

  if (digitalRead(LED_ESP)) {
    digitalWrite(LED_ESP, LOW);
    digitalWrite(LED_TCP, LOW);
    digitalWrite(OUT_1, LOW);
    digitalWrite(OUT_2, LOW);
    digitalWrite(OUT_3, LOW);
    digitalWrite(OUT_4, LOW);
  }
  else {
    digitalWrite(LED_ESP, HIGH);
    digitalWrite(LED_TCP, HIGH);
    digitalWrite(OUT_1, HIGH);
    digitalWrite(OUT_2, HIGH);
    digitalWrite(OUT_3, HIGH);
    digitalWrite(OUT_4, HIGH);
  }
}

