package net.ionoff.webhook;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;

@SpringBootApplication
public class WebhookApplication extends SpringBootServletInitializer {

    private Logger logger = LoggerFactory.getLogger(this.getClass());

    @Override
    protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
        return application.sources(WebhookApplication.class);
    }

    public static void main(String[] args) {
        SpringApplication.run(WebhookApplication.class, args);
    }
}
