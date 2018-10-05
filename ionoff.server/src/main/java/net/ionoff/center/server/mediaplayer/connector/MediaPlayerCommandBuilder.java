package net.ionoff.center.server.mediaplayer.connector;

import net.ionoff.center.server.broker.BrokerCommand;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;

import java.util.Map;
import java.util.UUID;

public class MediaPlayerCommandBuilder {

    public static BrokerCommand buildCommandGetStatus(MediaPlayer player, Map<String, Object> params) {
        BrokerCommand brokerCommand = new BrokerCommand();
        brokerCommand.setAddress(player.getMac());
        brokerCommand.setProtocol("mqtt");
        String subscription = UUID.randomUUID().toString();
        brokerCommand.setSubscription(subscription);
        params.put("subscription", subscription);
        brokerCommand.setContent(params);
        return brokerCommand;
    }

}
