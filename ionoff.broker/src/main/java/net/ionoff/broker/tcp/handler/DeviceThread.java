package net.ionoff.broker.tcp.handler;

import net.ionoff.broker.mqtt.MqttBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;

public class DeviceThread extends Thread {

    private static final Logger LOGGER = LoggerFactory.getLogger(MqttBroker.class);

    private boolean quit;
    private final MqttBroker mqttBroker;
    private Device device;

    DeviceThread(Device device, MqttBroker mqttBroker) {
        this.mqttBroker = mqttBroker;
        this.device = device;
    }

    @Override
    public void run() {
        for (; true; ) {
            if (quit) {
                return;
            }
            try {
                sleep(5000);
                update();
            } catch (InterruptedException e) {
                LOGGER.error("InterruptedException " + e.getMessage());
            } catch (Throwable t) {
                LOGGER.error(t.getMessage(), t);
            }
        }
    }

    private void update() {

    }

    void setDevice(Device device) {
        this.device = device;
    }

    public String getUid() {
        return device.getUid();
    }

    public void shutdown() {
        quit = true;
    }
}
