package net.ionoff.center.server.entity;

public class QueryCriteria {
	
	private Long projectId;
	private String searchKey;
	private String searchField;
	private Integer fromIndex;
	private Integer maxResults;
	private String sortBy;
	private Boolean isAscending;
	
	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
	
	public String getSearchKey() {
		return searchKey;
	}
	public void setSearchKey(String searchKey) {
		this.searchKey = searchKey;
	}
	
	public String getSearchField() {
		return searchField;
	}
	public void setSearchField(String searchField) {
		this.searchField = searchField;
	}
	
	public Integer getFromIndex() {
		return fromIndex;
	}
	public void setFromIndex(Integer fromIndex) {
		this.fromIndex = fromIndex;
	}
	
	public Integer getMaxResults() {
		return maxResults;
	}
	public void setMaxResults(Integer maxResults) {
		this.maxResults = maxResults;
	}
	
	public String getSortBy() {
		return sortBy;
	}
	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}
	
	public Boolean getIsAscending() {
		return isAscending;
	}
	public void setIsAscending(Boolean isAscending) {
		this.isAscending = isAscending;
	}
	
	public boolean isBlankKey() {
		return searchKey == null || searchKey.isEmpty();
	}
	
}
