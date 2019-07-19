package net.xapxinh.player.connection;

import java.util.Map;

import org.json.JSONException;

import com.google.gson.Gson;

public class TcpRequest {
	
	private static final Gson GSON = new Gson();;
	private Map<String, String> params;

	public TcpRequest(String request) throws JSONException {
		params = GSON.fromJson(request, Map.class);
	}

	public String getContext() {
		return params.get("context");
	}

	public String getCommand(){
		return params.get("command");
	}
	
	public String getParameter(String parametter) {
		return params.get(parametter);
	}
	
	public Map<String, String> getParameters() {
		return params;
	}
}
