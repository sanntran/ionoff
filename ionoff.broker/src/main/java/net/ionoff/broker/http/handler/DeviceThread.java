package net.ionoff.broker.http.handler;

import com.google.gson.Gson;
import net.ionoff.broker.http.HttpClient;
import net.ionoff.broker.mqtt.MqttBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DeviceThread extends Thread {

    private static final Logger LOGGER = LoggerFactory.getLogger(MqttBroker.class);
    private static final Gson GSON = new Gson();

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
        if (device == null || device.getUrls() == null || device.getUrls().isEmpty()
                || device.getTopic() == null || device.getTopic().isEmpty()) {
            return;
        }
        List<String> responses = new ArrayList<>();
        for (String url : device.getUrls()) {
            try {
                String response = HttpClient.sendGetRequest(url);
                responses.add(response);
            } catch (Exception e) {
                LOGGER.error(e.getClass().getSimpleName() + " GET " + url + " " + e.getMessage());
                String response = "Error:" + e.getMessage();
                responses.add(response);
            }
        }
        Map<String, Object> map = new HashMap<>();
        map.put("uid", device.getUid());
        map.put("status", responses);
        mqttBroker.publishMessage(device.getTopic(), GSON.toJson(map));
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
