package net.ionoff.broker.tcp.handler;

import com.google.gson.Gson;
import net.ionoff.broker.http.HttpClient;
import net.ionoff.broker.http.HttpMethod;
import net.ionoff.broker.http.HttpRequest;
import net.ionoff.broker.http.HttpStatus;
import net.ionoff.broker.mqtt.MqttBroker;
import net.ionoff.broker.mqtt.MqttRequest;
import net.ionoff.broker.tcp.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HttpHandler {
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpHandler.class);

    private static final Gson GSON = new Gson();

    private static final String DEVICES = "/devices";
    private static final String COMMANDS = "/commands";

    private final MqttBroker mqttBroker;
    private final TcpBroker tcpBroker;
    private final DeviceManager deviceManager;

    public HttpHandler(TcpBroker tcpBroker, MqttBroker mqttBroker) {
        this.tcpBroker = tcpBroker;
        this.mqttBroker = mqttBroker;
        this.deviceManager = new DeviceManager(mqttBroker);
    }

    public ResponseBody handleHttpRequest(HttpRequest httpRequest) {
        String path = httpRequest.getUri();
        if (path.equals(DEVICES)) {
            String body = httpRequest.getBody();
            try {
                if (HttpMethod.DELETE.equals(httpRequest.getMethod())) {
                    Device device = GSON.fromJson(body, Device.class);
                    deviceManager.removeDevice(device);
                }
                else if (HttpMethod.PUT.equals(httpRequest.getMethod())) {
                    Device device = GSON.fromJson(body, Device.class);
                    deviceManager.addDevice(device);
                }
                else if (HttpMethod.POST.equals(httpRequest.getMethod())) {
                    DeviceList devices = GSON.fromJson(body, DeviceList.class);
                    deviceManager.setDevices(devices);
                }
                else {
                    String message = "Method not allowed " + httpRequest.getMethod().name();
                    throw new ClientException(HttpStatus.METHOD_NOT_ALLOWED, message);
                }
                String resp = "";
                return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), resp);
            } catch (ClientException e) {
                throw e;
            } catch (Exception e) {
                String msg = "Request is invalid, " + e.getMessage();
                throw new ClientException(HttpStatus.BAD_REQUEST, msg);
            }
        }
        else if (path.equals(COMMANDS)) {
            String body = httpRequest.getBody();
            Command command = toRequestModel(body);
            if ("http".equals(command.getProtocol())) {
                String resp = sendHttpRequest(command.getAddress());
                return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), resp);
            }
            else if ("tcp".equals(command.getProtocol())) {
                String resp = sendTcpCommand(command.getAddress(), command.getContent());
                return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), resp);
            }
            else if ("mqtt".equals(command.getProtocol())) {
                String resp = sendMqttMessage(command.getAddress(), command.getContent());
                return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), resp);
            }
            else {
                String message = "Unknown protocol " + command.getProtocol();
                throw new ClientException(HttpStatus.BAD_REQUEST, message);
            }
        }
        else {
            throw new ClientException(HttpStatus.NOT_FOUND, path);
        }
    }


    private String sendTcpCommand(String address, String content) {
        try {
            return tcpBroker.sendCommand(address, content);
        } catch (Exception e) {
            String msg = "Error sending tcp command to " + address + ": " + e.getClass().getSimpleName() + " " + e.getMessage();
            LOGGER.error(msg, e);
            throw new ServerException(msg);
        }
    }

    private String sendMqttMessage(String address, String content) {
        try {
            return new MqttRequest(mqttBroker).sendMqttRequest(address, content);
        } catch (Exception e) {
            String msg = "Error sending mqtt request to " + address + ": " + e.getClass().getSimpleName() + " " + e.getMessage();
            LOGGER.error(e.getMessage(), e);
            throw new ServerException(msg);
        }
    }

    private String sendHttpRequest(String address) {
        try {
            return HttpClient.sendGetRequest(address);
        } catch (Exception e) {
            String msg = "Error sending http request to " + address + ": " + e.getClass().getSimpleName() + " " + e.getMessage();
            LOGGER.error(msg, e);
            throw new ServerException(msg);
        }
    }

    private Command toRequestModel(String body) {
        try {
            return GSON.fromJson(body, Command.class);
        } catch (Exception e) {
            String msg = "Invalid request body. Cannot parse to object model";
            LOGGER.error(msg, e);
            throw new ClientException(HttpStatus.BAD_REQUEST, msg);
        }
    }

}
