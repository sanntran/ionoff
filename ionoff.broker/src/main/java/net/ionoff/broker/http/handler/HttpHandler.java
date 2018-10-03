package net.ionoff.broker.http.handler;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import net.ionoff.broker.http.*;
import net.ionoff.broker.mqtt.MqttBroker;
import net.ionoff.broker.mqtt.MqttRequest;
import net.ionoff.broker.tcp.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

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
                    return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), device);
                }
                else if (HttpMethod.PUT.equals(httpRequest.getMethod())) {
                    Device device = GSON.fromJson(body, Device.class);
                    deviceManager.addDevice(device);
                    return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), device);
                }
                else if (HttpMethod.POST.equals(httpRequest.getMethod())) {
                    DeviceList devices = GSON.fromJson(body, DeviceList.class);
                    deviceManager.setDevices(devices);
                    return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), devices);
                }
                else {
                    String message = "Method not allowed " + httpRequest.getMethod().name();
                    throw new ClientException(HttpStatus.METHOD_NOT_ALLOWED, message);
                }
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
                Object resp = sendHttpRequest(command.getMethod(), command.getAddress(), getContent(command));
                return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), resp);
            }
            else if ("tcp".equals(command.getProtocol())) {
                Object resp = sendTcpCommand(command.getAddress(), getContent(command));
                return new ResponseBody(HttpStatus.OK.getStatus(), HttpStatus.OK.getDescription(), resp);
            }
            else if ("mqtt".equals(command.getProtocol())) {
                Object resp = sendMqttMessage(command.getAddress(), command.getSubscription(),
                        command.getKeyword(), getContent(command));
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

    private String getContent(Command command) {
        if (command.getContent() instanceof String) {
            return (String) command.getContent();
        }
        else {
            return GSON.toJson(command.getContent());
        }
    }

    private Object sendTcpCommand(String address, String content) {
        try {
            String resp =  tcpBroker.sendCommand(address, content);
            try {
                return GSON.fromJson(resp, JsonObject.class);
            } catch (Exception e) {
                return resp;
            }
        } catch (Exception e) {
            String msg = "Error sending tcp command to " + address + ": " + e.getClass().getSimpleName() + " " + e.getMessage();
            LOGGER.error(msg, e);
            throw new ServerException(msg);
        }
    }

    private Object sendMqttMessage(String address, String subscription,
                                   String keyword, String content) {
        try {
            String resp = new MqttRequest(mqttBroker, subscription, keyword).sendMqttRequest(address, content);
            try {
                return GSON.fromJson(resp, JsonObject.class);
            } catch (Exception e) {
                return resp;
            }
        } catch (Exception e) {
            String msg = "Error sending mqtt request to " + address + ": " + e.getClass().getSimpleName() + " " + e.getMessage();
            LOGGER.error(e.getMessage(), e);
            throw new ServerException(msg);
        }
    }

    private Object sendHttpRequest(String method, String address, String content) {
        try {
            String resp = HttpClient.sendGetRequest(address);
            try {
                return GSON.fromJson(resp, JsonObject.class);
            } catch (Exception e) {
                return resp;
            }
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
