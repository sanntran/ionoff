package net.ionoff.broker;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import net.ionoff.broker.mqtt.MqttBroker;
import net.ionoff.broker.tcp.TcpBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.function.Consumer;

public class MainRunner {

    private static final Logger LOGGER = LoggerFactory.getLogger(MainRunner.class);

    private static final String MQTT_DIR = "ibroker";
    private static final String MQTT_JAVA_DIR = MQTT_DIR + "/src/main/java/";

    public static void main(String[] args) {
        System.out.println("Log is written to: " + System.getProperty("user.dir"));
        MainRunner.run(MqttBroker.class);
    }

    public static void run(Class clazz) {
        run(MQTT_JAVA_DIR, clazz, new VertxOptions().setClustered(false), null);
    }

    public static void run(String exampleDir, Class clazz, VertxOptions options, DeploymentOptions
            deploymentOptions) {
        run(exampleDir + clazz.getPackage().getName().replace(".", "/"), clazz.getName(), options, deploymentOptions);
    }


    public static void run(String exampleDir, String verticleID, VertxOptions options, DeploymentOptions deploymentOptions) {
        if (options == null) {
            // Default parameter
            options = new VertxOptions();
        }
        // Smart cwd detection

        // Based on the current directory (.) and the desired directory (exampleDir), we try to compute the vertx.cwd
        // directory:
        try {
            // We need to use the canonical file. Without the file name is .
            File current = new File(".").getCanonicalFile();
            if (exampleDir.startsWith(current.getName())  && ! exampleDir.equals(current.getName())) {
                exampleDir = exampleDir.substring(current.getName().length() + 1);
            }
        } catch (IOException e) {
            LOGGER.error("Ignore error: " + e.getMessage());
        }

        System.setProperty("vertx.cwd", exampleDir);
        Consumer<Vertx> runner = vertx -> {
            try {
                if (deploymentOptions != null) {
                    vertx.deployVerticle(verticleID, deploymentOptions);
                } else {
                    vertx.deployVerticle(verticleID);
                }
            } catch (Throwable t) {
                LOGGER.error(t.getMessage(), t);
            }
        };
        if (options.isClustered()) {
            Vertx.clusteredVertx(options, res -> {
                if (res.succeeded()) {
                    Vertx vertx = res.result();
                    runner.accept(vertx);
                } else {
                    res.cause().printStackTrace();
                }
            });
        } else {
            Vertx vertx = Vertx.vertx(options);
            runner.accept(vertx);
        }
    }
}