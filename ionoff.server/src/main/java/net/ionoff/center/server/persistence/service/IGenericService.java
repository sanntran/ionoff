package net.ionoff.center.server.persistence.service;

import java.io.Serializable;
import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.IPersistence;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

@Transactional
public interface IGenericService<T extends Serializable, D extends Serializable> extends IPersistence<T> {
	
	T requireById(long id);
	
	D requireDtoById(long id);
	
	D insertDto(User user, D dto);
	
	D updateDto(User user, D dto);
	
	void deleteDtoById(User user, long id);

	long countByCriteria(QueryCriteriaDto queryCriteriaDto);
	
	List<D> searchByCriteria(QueryCriteriaDto queryCriteriaDto);

}
