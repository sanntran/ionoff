package net.ionoff.broker.http.handler;

import net.ionoff.broker.mqtt.MqttBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class DeviceManager {

    private static final Logger LOGGER = LoggerFactory.getLogger(MqttBroker.class);

    private final MqttBroker mqttBroker;
    private final List<DeviceThread> deviceThreads;

    DeviceManager(MqttBroker mqttBroker) {
        this.mqttBroker = mqttBroker;
        this.deviceThreads = Collections.synchronizedList(new ArrayList<>());
    }

    synchronized void addDevice(Device device) {
        boolean existed = false;
        for (int i = 0; i < deviceThreads.size(); i++) {
            if (deviceThreads.get(i).getUid().equals(device.getUid())) {
                existed = true;
                deviceThreads.get(i).setDevice(device);
                break;
            }
        }
        if (!existed) {
            DeviceThread thread = new DeviceThread(device, mqttBroker);
            deviceThreads.add(thread);
            thread.start();
        }
    }

    void removeDevice(Device device) {
        for (int i = 0; i < deviceThreads.size(); i++) {
            if (deviceThreads.get(i).getUid().equals(device.getUid())) {
                deviceThreads.remove(i);
                break;
            }
        }
    }

    void setDevices(DeviceList devices) {
        for (int i = deviceThreads.size() - 1; i >= 0; i--) {
            deviceThreads.get(i).shutdown();
            deviceThreads.remove(i);
        }
        for (Device device : devices) {
            addDevice(device);
        }
    }
}
