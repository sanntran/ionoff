package net.ionoff.center.server.persistence.mapper;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

public class QueryCriteriaMapper {

	public static QueryCriteria toQueryCriteria(QueryCriteriaDto queryCriteriaDto) {
		final QueryCriteria criteria = new QueryCriteria();
		criteria.setDeviceId(queryCriteriaDto.getDeviceId());
		criteria.setProjectId(queryCriteriaDto.getProjectId());
		criteria.setSearchKey(queryCriteriaDto.getSearchKey());
		criteria.setSearchField(queryCriteriaDto.getSearchField());
		criteria.setFromIndex(queryCriteriaDto.getFromIndex());
		criteria.setMaxResults(queryCriteriaDto.getMaxResults());
		criteria.setSortBy(queryCriteriaDto.getSortBy());
		criteria.setIsAscending(queryCriteriaDto.getIsAscending());
		return criteria;
	}
	
}
