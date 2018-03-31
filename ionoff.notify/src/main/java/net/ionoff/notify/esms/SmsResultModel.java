package net.ionoff.notify.esms;

public class SmsResultModel {
	
	private String codeResult;
	private String smsId;
	private String countRegenerate;
	private String isSandbox;
	
	public String getCodeResult() {
		return codeResult;
	}
	public void setCodeResult(String codeResult) {
		this.codeResult = codeResult;
	}
	
	public String getSmsId() {
		return smsId;
	}
	public void setSmsId(String smsId) {
		this.smsId = smsId;
	}
	
	public String getCountRegenerate() {
		return countRegenerate;
	}
	public void setCountRegenerate(String countRegenerate) {
		this.countRegenerate = countRegenerate;
	}
	
	public String getIsSandbox() {
		return isSandbox;
	}
	public void setIsSandbox(String isSandbox) {
		this.isSandbox = isSandbox;
	}
}
