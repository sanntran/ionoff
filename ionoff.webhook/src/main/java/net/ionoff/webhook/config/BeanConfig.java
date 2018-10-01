package net.ionoff.webhook.config;

import net.ionoff.webhook.mapper.CenterMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class BeanConfig {

    @Bean
    public CenterMapper centerMapper() {
        return new CenterMapper();
    }
}
