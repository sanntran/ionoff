package net.ionoff.notify.esms;

public class SmsRequestBody {
	
	private String apiKey;
	private String secretKey;
	private String[] subcribers;
	private String message;
	
	public SmsRequestBody(String apiKey, String secretKey, String[] subcribers, String message) {
		this.apiKey = apiKey;
		this.secretKey = secretKey;
		this.subcribers = subcribers;
		this.message = message;
	}
	
	public String toXmlFormat() {
		StringBuilder builder = new StringBuilder();
		builder.append("<RQST>");
		builder.append("<APIKEY>").append(apiKey).append("</APIKEY>");
		builder.append("<SECRETKEY>").append(secretKey).append("</SECRETKEY>");
		builder.append("<ISFLASH>").append("0").append("</ISFLASH>");
		builder.append("<UNICODE>").append("0").append("</UNICODE>");
		builder.append("<SMSTYPE>").append("4").append("</SMSTYPE>");
		builder.append("<CONTENT>").append(message).append("</CONTENT>");
		builder.append("<CONTACTS>");
		for (String subcriber : subcribers) {
			builder.append("<CUSTOMER>");
			builder.append("<PHONE>").append(subcriber.replace(" ", "")).append("</PHONE>");
			builder.append("</CUSTOMER>");
		}
		builder.append("</CONTACTS>");
		builder.append("</RQST>");
		return builder.toString();
	}
}
