package net.ionoff.webhook.dto;

import java.util.List;

public class QueryResult {
	
	private String queryText;
	private String languageCode;
	private Number speechRecognitionConfidence;
	private String action;
	private Object parameters;
	private Boolean allRequiredParamsPresent;
	private String fulfillmentText;
	private List<Object> fulfillmentMessages;
	private String webhookSource;
	private Object webhookPayload;
	private List<Object> outputContexts;
	private Object intent;
	private Number intentDetectionConfidence;
	private Object diagnosticInfo;
	
	public String getQueryText() {
		return queryText;
	}
	public void setQueryText(String queryText) {
		this.queryText = queryText;
	}
	
	public String getLanguageCode() {
		return languageCode;
	}
	
	public void setLanguageCode(String languageCode) {
		this.languageCode = languageCode;
	}
	public Number getSpeechRecognitionConfidence() {
		return speechRecognitionConfidence;
	}
	public void setSpeechRecognitionConfidence(Number speechRecognitionConfidence) {
		this.speechRecognitionConfidence = speechRecognitionConfidence;
	}
	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
	}
	public Object getParameters() {
		return parameters;
	}
	public void setParameters(Object parameters) {
		this.parameters = parameters;
	}
	public Boolean getAllRequiredParamsPresent() {
		return allRequiredParamsPresent;
	}
	public void setAllRequiredParamsPresent(Boolean allRequiredParamsPresent) {
		this.allRequiredParamsPresent = allRequiredParamsPresent;
	}
	public String getFulfillmentText() {
		return fulfillmentText;
	}
	public void setFulfillmentText(String fulfillmentText) {
		this.fulfillmentText = fulfillmentText;
	}
	public List<Object> getFulfillmentMessages() {
		return fulfillmentMessages;
	}
	public void setFulfillmentMessages(List<Object> fulfillmentMessages) {
		this.fulfillmentMessages = fulfillmentMessages;
	}
	public String getWebhookSource() {
		return webhookSource;
	}
	public void setWebhookSource(String webhookSource) {
		this.webhookSource = webhookSource;
	}
	public Object getWebhookPayload() {
		return webhookPayload;
	}
	public void setWebhookPayload(Object webhookPayload) {
		this.webhookPayload = webhookPayload;
	}
	public List<Object> getOutputContexts() {
		return outputContexts;
	}
	public void setOutputContexts(List<Object> outputContexts) {
		this.outputContexts = outputContexts;
	}
	public Object getIntent() {
		return intent;
	}
	public void setIntent(Object intent) {
		this.intent = intent;
	}
	public Number getIntentDetectionConfidence() {
		return intentDetectionConfidence;
	}
	public void setIntentDetectionConfidence(Number intentDetectionConfidence) {
		this.intentDetectionConfidence = intentDetectionConfidence;
	}
	public Object getDiagnosticInfo() {
		return diagnosticInfo;
	}
	public void setDiagnosticInfo(Object diagnosticInfo) {
		this.diagnosticInfo = diagnosticInfo;
	}
	
}
