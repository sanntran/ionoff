package net.ionoff.broker.tcp.handler;

import com.google.gson.Gson;
import net.ionoff.broker.mqtt.MqttBroker;
import net.ionoff.broker.mqtt.MqttClient;
import net.ionoff.broker.tcp.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HttpHandler {
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpHandler.class);

    private static final Gson GSON = new Gson();

    private static final String RELAY_DRIVER_PATH = "/relaydrivers/";
    private static final String SENSOR_DRIVER_PATH = "/sensordrivers/";
    private static final String MEDIA_PLAYER_PATH = "/mediaplayers/";

    private final MqttBroker mqttBroker;
    private final TcpBroker tcpBroker;

    public HttpHandler(TcpBroker tcpBroker, MqttBroker mqttBroker) {
        this.tcpBroker = tcpBroker;
        this.mqttBroker = mqttBroker;
    }

    public ResponseBody handleHttpRequest(HttpRequest httpRequest) {
        String path = httpRequest.getUri();
        if (path.startsWith(RELAY_DRIVER_PATH) && path.length() > RELAY_DRIVER_PATH.length()) {
            return handleRelayDriverRequest(httpRequest);
        }
        else if (path.startsWith(SENSOR_DRIVER_PATH) && path.length() > SENSOR_DRIVER_PATH.length()) {
            return handleSensorDriverRequest(httpRequest);
        }
        else if (path.startsWith(MEDIA_PLAYER_PATH) && path.length() > MEDIA_PLAYER_PATH.length()) {
            return handleMediaPlayerRequest(httpRequest);
        }
        else {
            throw new ClientException(Status.NOT_FOUND, path);
        }
    }

    private ResponseBody handleSensorDriverRequest(HttpRequest httpRequest) {
        throw new ClientException(Status.NOT_FOUND, httpRequest.getUri());
    }

    private ResponseBody handleMediaPlayerRequest(HttpRequest httpRequest) {
        throw new ClientException(Status.NOT_FOUND, httpRequest.getUri());
    }

    private ResponseBody handleRelayDriverRequest(HttpRequest httpRequest) {
        String body = httpRequest.getBody();
        RelayDriverMsg request = toRequestModel(body);
        if ("http".equals(request.getProtocol())) {
            String resp = sendHttpRequest(request.getAddress());
            return new ResponseBody(Status.OK.getStatus(), Status.OK.getDescription(), resp);
        }
        else if ("tcp".equals(request.getProtocol())) {
            String resp = sendTcpCommand(request.getAddress(), request.getContent());
            return new ResponseBody(Status.OK.getStatus(), Status.OK.getDescription(), resp);
        }
        else if ("mqtt".equals(request.getProtocol())) {
            String resp = sendMqttMessage(request.getAddress(), request.getContent());
            return new ResponseBody(Status.OK.getStatus(), Status.OK.getDescription(), resp);
        }
        else {
            String message = "Unknown protocol " + request.getProtocol();
            throw new ClientException(Status.BAD_REQUEST, message);
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
            return new MqttClient(mqttBroker).sendMqttRequest(address, content);
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

    private RelayDriverMsg toRequestModel(String body) {
        try {
            return GSON.fromJson(body, RelayDriverMsg.class);
        } catch (Exception e) {
            String msg = "Invalid request body. Cannot parse to object model";
            LOGGER.error(msg, e);
            throw new ClientException(Status.BAD_REQUEST, msg);
        }
    }

}
