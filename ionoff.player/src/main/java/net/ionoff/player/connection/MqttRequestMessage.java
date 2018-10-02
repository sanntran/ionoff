package net.ionoff.player.connection;

import com.google.gson.Gson;
import org.json.JSONException;

import java.util.Map;

public class MqttRequestMessage {

	private static final Gson GSON = new Gson();
	private Map<String, String> params;
	
	public MqttRequestMessage(String request) throws JSONException {
		params = GSON.fromJson(request, Map.class);
	}

	public String getSubscription() throws JSONException {
		return params.get("subscription");
	}

	public String getContext() throws JSONException {
		return params.get("context");
	}

	public String getCommand() throws JSONException {
		return params.get("command");
	}
	
	public String getParameter(String parametter) throws JSONException {
		return params.get(parametter);
	}
	
	public Map<String, String> getParameters() {
		return params;
	}
}
