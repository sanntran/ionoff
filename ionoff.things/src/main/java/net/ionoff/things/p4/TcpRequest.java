package net.ionoff.things.p4;

public class TcpRequest {
	private String obj;
	private String method;
	private String attribute;
	private String value;
	
	public TcpRequest setIOObj() {
		obj = "io";
		return this;
	}
	
	public TcpRequest setCfObj() {
		obj = "cf";
		return this;
	}
	
	public TcpRequest setGetMethod() {
		method = "get";
		return this;
	}
	
	public TcpRequest setSetMethod() {
		method = "set";
		return this;
	}
	
	public TcpRequest setTnputTypeAtt() {
		attribute = "in";
		return this;
	}
	
	public TcpRequest setServerIpAtt() {
		attribute = "sv";
		return this;
	}
	
	public TcpRequest setIpAtt() {
		attribute = "ip";
		return this;
	}
	
	public TcpRequest setPwAtt() {
		attribute = "pw";
		return this;
	}
	
	public TcpRequest setSubnetMaskAtt() {
		attribute = "sm";
		return this;
	}
	
	public TcpRequest setGatewayAtt() {
		attribute = "gw";
		return this;
	}
	
	public TcpRequest setMacAtt() {
		attribute = "ma";
		return this;
	}
	
	public TcpRequest setAttValue(String attValue) {
		if (attValue == null || attValue.isEmpty()) {
			throw new IllegalArgumentException();
		}
		StringBuilder b = new StringBuilder();
		if ("ma".equals(attribute)) {
			String macs[] = attValue.replaceAll("\\.", "-").split("-");
			for (int i = 0; i < 4; i++) {
				int m = Integer.parseInt(macs[i], 16);
				b.append(m);
				if (i != 3) {
					b.append('-');
				}
			}
			value = b.toString();
			
		}
		else {
			value = attValue.replaceAll("\\.", "-");
		}
		return this;
	}
	
	public String build() {
		if(obj == null || method == null) {
			throw new IllegalArgumentException("obj");
		}
		if ("set".equals(method)) {
			if (attribute == null || value == null) {
				throw new IllegalArgumentException("set");
			}
		}
		StringBuilder builder = new StringBuilder();
		builder.append('{').append(obj).append(method);
		
		if ("get".equals(method)) {
			builder.append('}');
		}
		else {
			builder.append(attribute).append(value).append('}');
		}
		return builder.toString();
	}

	public TcpRequest seOutAtt(int relayIndex) {
		this.attribute = "o" + relayIndex;
		return this;
	}
}
