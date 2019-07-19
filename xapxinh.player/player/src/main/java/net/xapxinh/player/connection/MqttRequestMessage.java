package net.xapxinh.player.connection;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.json.JSONException;

import java.util.Map;

public class MqttRequestMessage {

	private static final Gson GSON = new Gson();
	private JsonObject params;

	public MqttRequestMessage(String request) {
		params = GSON.fromJson(request, JsonObject.class);
	}

	public JsonObject asJsonObject() {
		return params;
	}

	public String getSubscription() {
		return getAsString("subscription");
	}

	public String getAsString(String key) {
		JsonElement element = params.get(key);
		if (element == null) {
			return null;
		}
		return element.getAsString();
	}

	public JsonElement getAsJsonElement(String key) {
		return params.get(key);
	}

	public String getContext() {
		return getAsString("context");
	}

	public String getCommand() {
		return getAsString("command");
	}

}
