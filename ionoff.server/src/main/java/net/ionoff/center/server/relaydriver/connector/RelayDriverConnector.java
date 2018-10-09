package net.ionoff.center.server.relaydriver.connector;

import static net.ionoff.center.server.relaydriver.connector.RelayDriverCommandBuilder.buildCommandCloseRelay;
import static net.ionoff.center.server.relaydriver.connector.RelayDriverCommandBuilder.buildCommandOpenRelay;

import java.util.Arrays;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.client.RestTemplate;

import net.ionoff.center.server.broker.BrokerClientFactory;
import net.ionoff.center.server.broker.BrokerCommand;
import net.ionoff.center.server.broker.BrokerHttpClient;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.relaydriver.exception.RelayDriverConnectException;
import net.ionoff.center.server.relaydriver.exception.RelayDriverRequestException;
import net.ionoff.center.server.wsclient.RestTemplateFactory;
import net.ionoff.center.server.wsclient.RestTemplateRequestIntercepter;

public class RelayDriverConnector {

    private static final Logger LOGGER = Logger.getLogger(RelayDriverConnector.class.getName());

    private final BrokerHttpClient brokerHttpClient;

    @Autowired
    public RelayDriverConnector(BrokerClientFactory brokerClientFactory) {

        RestTemplate restClient = RestTemplateFactory.buildRestTemplate(
                new RelayDriverRequestExceptionHandler(),
                Arrays.asList(new RestTemplateRequestIntercepter()));
        brokerHttpClient = brokerClientFactory.createHttpClient(restClient);
    }

    public String openRelay(RelayDriver relayDriver, int relayIndex) {
        return sendCommand(relayDriver, buildCommandOpenRelay(relayDriver, relayIndex));
    }

    public String closeRelay(RelayDriver relayDriver, int relayIndex) {
        return sendCommand(relayDriver, buildCommandCloseRelay(relayDriver, relayIndex));
    }

    public String openRelay(RelayDriver relayDriver, int relayIndex, Integer autoRevvert) {
        return sendCommand(relayDriver, buildCommandOpenRelay(relayDriver, relayIndex, autoRevvert));
    }

    public String closeRelay(RelayDriver relayDriver, int relayIndex, Integer autoRevvert) {
        return sendCommand(relayDriver, buildCommandCloseRelay(relayDriver, relayIndex, autoRevvert));
    }

    private String sendCommand(RelayDriver relayDriver, BrokerCommand command) {
        if (!relayDriver.isConnected()) {
            throw new RelayDriverConnectException(relayDriver.getName());
        }
        try {
            BrokerResponse brokerResponse = brokerHttpClient.sendCommand(command);
            String data = String.valueOf(brokerResponse.getData());
            LOGGER.info("Relay driver response: " + data);
            return data;
        } catch (RelayDriverRequestException e) {
            throw e;
        } catch (Exception e) {
            LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
            throw new RelayDriverRequestException(e.getClass().getSimpleName() + " " + e.getMessage());
        }
    }
}
