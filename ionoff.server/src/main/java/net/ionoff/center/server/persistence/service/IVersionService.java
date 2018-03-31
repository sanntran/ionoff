package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Version;
import net.ionoff.center.shared.dto.VersionDto;

@Transactional
public interface IVersionService extends IGenericService<Version, VersionDto> {
}
