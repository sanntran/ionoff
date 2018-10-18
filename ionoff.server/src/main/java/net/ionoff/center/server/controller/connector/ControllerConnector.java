package net.ionoff.center.server.controller.connector;

import static net.ionoff.center.server.controller.connector.ControllerCommandBuilder.buildCommandCloseRelay;
import static net.ionoff.center.server.controller.connector.ControllerCommandBuilder.buildCommandOpenRelay;

import java.util.Arrays;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import net.ionoff.center.server.broker.BrokerClientFactory;
import net.ionoff.center.server.broker.BrokerCommand;
import net.ionoff.center.server.broker.BrokerHttpClient;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.controller.exception.ControllerConnectException;
import net.ionoff.center.server.controller.exception.ControllerRequestException;
import net.ionoff.center.server.wsclient.RestTemplateFactory;
import net.ionoff.center.server.wsclient.RestTemplateRequestIntercepter;

@Component
public class ControllerConnector {

    private static final Logger LOGGER = LoggerFactory.getLogger(ControllerConnector.class.getName());

    private final BrokerHttpClient brokerHttpClient;

    @Autowired
    public ControllerConnector(BrokerClientFactory brokerClientFactory) {

        RestTemplate restClient = RestTemplateFactory.buildRestTemplate(
                new ControllerRequestExceptionHandler(),
                Arrays.asList(new RestTemplateRequestIntercepter()));
        brokerHttpClient = brokerClientFactory.createHttpClient(restClient);
    }

    public String openRelay(Controller controller, int relayIndex) {
        return sendCommand(controller, buildCommandOpenRelay(controller, relayIndex));
    }

    public String closeRelay(Controller controller, int relayIndex) {
        return sendCommand(controller, buildCommandCloseRelay(controller, relayIndex));
    }

    public String openRelay(Controller controller, int relayIndex, Integer autoRevvert) {
        return sendCommand(controller, buildCommandOpenRelay(controller, relayIndex, autoRevvert));
    }

    public String closeRelay(Controller controller, int relayIndex, Integer autoRevvert) {
        return sendCommand(controller, buildCommandCloseRelay(controller, relayIndex, autoRevvert));
    }

    private String sendCommand(Controller controller, BrokerCommand command) {
        if (!controller.isConnected()) {
            throw new ControllerConnectException(controller.getName());
        }
        try {
            BrokerResponse brokerResponse = brokerHttpClient.sendCommand(command);
            String data = String.valueOf(brokerResponse.getData());
            LOGGER.info("Relay driver response: " + data);
            return data;
        } catch (ControllerRequestException e) {
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
            throw new ControllerRequestException(e.getClass().getSimpleName() + " " + e.getMessage());
        }
    }
}
