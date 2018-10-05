package net.ionoff.center.server.persistence.mapper;

import net.ionoff.center.server.entity.IEntity;
import net.ionoff.center.shared.dto.IDto;

public interface ObjMapper<T extends IEntity, D extends IDto> {
	
	D createDto(T t);
}