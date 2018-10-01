package net.ionoff.webhook.mapper;

import net.ionoff.webhook.dto.CenterDto;
import net.ionoff.webhook.model.Center;

public class CenterMapper {

    public CenterDto createDto(Center model) {
        CenterDto dto = new CenterDto();
        dto.setId(model.getId());
        dto.setIp(model.getIp());
        dto.setTime(model.getTime());
        return dto;
    }

    public void updateModel(CenterDto dto, Center model) {
        model.setId(dto.getId());
        model.setIp(dto.getIp());
        model.setTime(dto.getTime());
    }
}
