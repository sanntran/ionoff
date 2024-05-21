package net.ionoff.center.server.controller.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SxSensor {


    private String type;
    private String value;
    private String addrSub;
    private Long timestamp;

}
