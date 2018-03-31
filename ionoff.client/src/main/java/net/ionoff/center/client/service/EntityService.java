package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.POST;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

public interface EntityService<T extends BaseDto> extends RestService {
	
	@POST
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<T>> callback);
	@POST
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	@DELETE
	void delete(Long entityId, MethodCallback<MessageDto> callback);
}
