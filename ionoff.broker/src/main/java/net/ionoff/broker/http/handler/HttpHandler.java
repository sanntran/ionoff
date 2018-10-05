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
import java.util.concurrent.TimeoutException;

public class HttpHandler {
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpHandler.class);

    private static final Gson GSON = new Gson();

    private static final String DEVICES = "/devices";
    private static final String COMMANDS = "/commands";

    private final MqttBroker mqttBroker;
    private final TcpBroker tcpBroker;

    public HttpHandler(TcpBroker tcpBroker, MqttBroker mqttBroker) {
        this.tcpBroker = tcpBroker;
        this.mqttBroker = mqttBroker;
    }

    public ResponseBody handleHttpRequest(HttpRequest httpRequest) throws TimeoutException {
        String path = httpRequest.getUri();
        if (path.equals(COMMANDS)) {
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
                                   String keyword, String content) throws TimeoutException {
        try {
            String resp = new MqttRequest(mqttBroker, subscription, keyword).sendMqttRequest(address, content);
            try {
                return GSON.fromJson(resp, JsonObject.class);
            } catch (Exception e) {
                return resp;
            }
        } catch (TimeoutException te) {
            String msg = te.getClass().getSimpleName() + ": timeout reading mqtt response from " + address;
            LOGGER.error(msg + " " + te.getMessage(), te);
            throw te;
        }
        catch (Exception e) {
            String msg = e.getClass().getSimpleName() + ": error sending mqtt request to " + address;
            LOGGER.error(msg + " " + e.getMessage(), e);
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
