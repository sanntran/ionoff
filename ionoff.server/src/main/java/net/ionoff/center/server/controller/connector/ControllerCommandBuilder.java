package net.ionoff.center.server.controller.connector;

import net.ionoff.center.server.broker.BrokerCommand;
import net.ionoff.center.server.entity.Controller;

public class ControllerCommandBuilder {

    public static BrokerCommand buildCommandCloseRelay(Controller controller, int relayIndex) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(controller.getProtocol());
        if ("http".equals(controller.getProtocol())) {
            command.setAddress(controller.getCommandCloseRelay(relayIndex));
        }
        else {
            command.setKeyword("code=okcmd");
            command.setAddress(controller.getKey());
            command.setContent(controller.getCommandCloseRelay(relayIndex));
        }
        return command;
    }

    public static BrokerCommand buildCommandOpenRelay(Controller controller, int relayIndex) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(controller.getProtocol());
        if ("http".equals(controller.getProtocol())) {
            command.setAddress(controller.getCommandOpenRelay(relayIndex));
        }
        else {
            command.setKeyword("code=okcmd");
            command.setAddress(controller.getKey());
            command.setContent(controller.getCommandOpenRelay(relayIndex));
        }
        return command;
    }

    public static BrokerCommand buildCommandCloseRelay(Controller controller, int relayIndex, Integer autoRevert) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(controller.getProtocol());
        if (autoRevert != null && Controller.ONE_SECOND == autoRevert.intValue()) {
        	command.setDelay(controller.getOneSecondDelay());
        }
        if ("http".equals(controller.getProtocol())) {
            command.setAddress(controller.getCommandCloseRelay(relayIndex, autoRevert));
        }
        else {
            command.setKeyword("code=okcmd");
            command.setAddress(controller.getKey());
            command.setContent(controller.getCommandCloseRelay(relayIndex, autoRevert));
        }
        return command;
    }

    public static BrokerCommand buildCommandOpenRelay(Controller controller, int relayIndex, Integer autoRevert) {
        BrokerCommand command = new BrokerCommand();
        command.setProtocol(controller.getProtocol());
        if (autoRevert != null && Controller.ONE_SECOND == autoRevert.intValue()) {
        	command.setDelay(controller.getOneSecondDelay());
        }
        if ("http".equals(controller.getProtocol())) {
            command.setAddress(controller.getCommandOpenRelay(relayIndex, autoRevert));
        }
        else {
            command.setKeyword("code=okcmd");
            command.setAddress(controller.getKey());
            command.setContent(controller.getCommandOpenRelay(relayIndex, autoRevert));
        }
        return command;
    }
}
