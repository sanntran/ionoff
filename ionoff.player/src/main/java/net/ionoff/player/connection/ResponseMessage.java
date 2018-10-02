package net.ionoff.player.connection;

import org.json.JSONException;
import org.json.JSONObject;

import com.google.gson.Gson;

public class ResponseMessage {

	private String uid;
	private String status;
	private String message;
	private String object;	
	private String clazz;	
	private static final Gson GSON = new Gson();
	
	public ResponseMessage(String uid, String message, Object obj) {
		this.message = message;
		this.clazz = obj.getClass().getSimpleName();
		if (obj instanceof Exception) {			
			this.status = "error";
		}
		else {
			this.status = "success";
			if (obj instanceof String) {
				this.object = (String)obj;
			}
			else {
				this.object = GSON.toJson(obj);
			}
		}
	}
	
	public String toJSONString() {
		JSONObject json = new JSONObject();
		try {
			json.put("status", status);
			json.put("message", message);
			json.put("object", object);
			json.put("clazz", clazz);
			return json.toString();
		}
		catch (JSONException e) {
			return "{\"status\":\"error\",\"clazz\":\"JSONException\",\"message\":\"" + e.getMessage() + "\"}";
		}
	}
}
