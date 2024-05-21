package net.ionoff.center.server.controller.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.vavr.control.Try;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@NoArgsConstructor
public class SxIOStatus {

    private String id;
    private List<IOStatus> sensors;

    @Getter
    @Setter
    @NoArgsConstructor
    public static class IOStatus {
        private String type;
        private String value;
        private String addrSub;
        private Long timestamp;
    }

    private static ObjectMapper objectMapper = newObjectMapper();

    public static boolean accept(String payload) {
        return Try.of(() -> objectMapper.readValue(payload, SxIOStatus.class)).isSuccess();
    }

    public static SxIOStatus of(String payload) {
        return Try.of(() -> objectMapper.readValue(payload, SxIOStatus.class))
                .getOrElseThrow(() -> new IllegalArgumentException("Failed to desirialize to SxIOStatus "  + payload));
    }

    private static ObjectMapper newObjectMapper() {
        objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return objectMapper;
    }

    public String getControllerKey() {
        return sensors.stream().map(IOStatus::getAddrSub).collect(Collectors.joining("-"));
    }
}
