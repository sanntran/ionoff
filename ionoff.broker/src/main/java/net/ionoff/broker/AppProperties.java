package net.ionoff.broker;

import net.ionoff.broker.mqtt.MqttBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

public class AppProperties {

    private static final Logger LOGGER = LoggerFactory.getLogger(AppProperties.class);

    private static List<String> brokerTopics = new ArrayList<>();

    private AppProperties() {

        final Properties properties = new Properties();
        try {
            // load a properties file
            properties.load(Thread.currentThread().getContextClassLoader().getResourceAsStream("application.properties"));
        }
        catch (final IOException ex) {
            LOGGER.error("Error reading application.properties " + ex.getMessage(), ex);
        }

        String topics = properties.getProperty("broker_topics");
        if (topics != null) {
            brokerTopics.addAll(Arrays.asList(topics.split(",")));
        }
    }

    public static List<String> getBrokerTopics() {
        return brokerTopics;
    }

    public static String getTcpTopic() {
        return brokerTopics.isEmpty() ? "IOnOffNet" : brokerTopics.get(0);
    }
}
