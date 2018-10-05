package net.ionoff.center.server.relaydriver.connector;

import net.ionoff.center.server.broker.BrokerCommand;
import net.ionoff.center.server.entity.RelayDriver;

public class RelayDriverCommandBuilder {

    public static BrokerCommand buildCommandCloseRelay(RelayDriver relayDriver, int relayIndex) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(relayDriver.getProtocol());
        if ("http".equals(relayDriver.getProtocol())) {
            command.setAddress(relayDriver.getCommandCloseRelay(relayIndex));
        }
        else {
            command.setKeyword("code=resp");
            command.setAddress(relayDriver.getKey());
            command.setContent(relayDriver.getCommandCloseRelay(relayIndex));
        }
        return command;
    }

    public static BrokerCommand buildCommandOpenRelay(RelayDriver relayDriver, int relayIndex) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(relayDriver.getProtocol());
        if ("http".equals(relayDriver.getProtocol())) {
            command.setAddress(relayDriver.getCommandOpenRelay(relayIndex));
        }
        else {
            command.setKeyword("code=resp");
            command.setAddress(relayDriver.getKey());
            command.setContent(relayDriver.getCommandOpenRelay(relayIndex));
        }
        return command;
    }

    public static BrokerCommand buildCommandCloseRelay(RelayDriver relayDriver, int relayIndex, Integer autoRevert) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(relayDriver.getProtocol());
        if (autoRevert != null && autoRevert.intValue() == 1) {
            command.setDelay(autoRevert);
        }
        if ("http".equals(relayDriver.getProtocol())) {
            command.setAddress(relayDriver.getCommandCloseRelay(relayIndex, autoRevert));
        }
        else {
            command.setKeyword("code=resp");
            command.setAddress(relayDriver.getKey());
            command.setContent(relayDriver.getCommandCloseRelay(relayIndex, autoRevert));
        }
        return command;
    }

    public static BrokerCommand buildCommandOpenRelay(RelayDriver relayDriver, int relayIndex, Integer autoRevert) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(relayDriver.getProtocol());
        command.setDelay(0);
        if ("http".equals(relayDriver.getProtocol())) {
            command.setAddress(relayDriver.getCommandOpenRelay(relayIndex, autoRevert));
        }
        else {
            command.setKeyword("code=resp");
            command.setAddress(relayDriver.getKey());
            command.setContent(relayDriver.getCommandOpenRelay(relayIndex, autoRevert));
        }
        return command;
    }
}
