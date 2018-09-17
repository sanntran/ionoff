package net.ionoff.server.webhook.model;

import java.io.Serializable;

public class WebhookRequest implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private String session;
	private String responseId;
	private QueryResult queryResult;
	private Object originalDetectIntentRequest;
	
	public String getSession() {
		return session;
	}	
	public void setSession(String session) {
		this.session = session;
	}
	
	public String getResponseId() {
		return responseId;
	}
	public void setResponseId(String responseId) {
		this.responseId = responseId;
	}
	
	public QueryResult getQueryResult() {
		return queryResult;
	}	
	public void setQueryResult(QueryResult queryResult) {
		this.queryResult = queryResult;
	}
	
	public Object getOriginalDetectIntentRequest() {
		return originalDetectIntentRequest;
	}
	public void setOriginalDetectIntentRequest(Object originalDetectIntentRequest) {
		this.originalDetectIntentRequest = originalDetectIntentRequest;
	}
	
}
