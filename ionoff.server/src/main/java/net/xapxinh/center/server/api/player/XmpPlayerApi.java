package net.xapxinh.center.server.api.player;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.server.service.player.AbstractPlayerConnectionPool;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.Schedule;
import net.xapxinh.center.shared.dto.Status;

public class XmpPlayerApi implements IPlayerApi {
	
	private final Gson gson;
	
	private final Type typeOfMediaFileList = new TypeToken<ArrayList<MediaFile>>(){}.getType();
	
	@Autowired
	private AbstractPlayerConnectionPool playerConnectionPool;

	private PlayerConnection getPlayerConnection(Player player) {
		return playerConnectionPool.getPlayerConnection(player.getId());
	}

	public XmpPlayerApi() {
		gson = new Gson();
	}

	private Map<String, Object> putContext(Map<String, Object> params, String context) {
		params.put("context", context);
		return params;
	}

	@Override
	public Status requestStatus(Player player, Map<String, Object> params) throws PlayerConnectException {
		try {
			final String statusJson = requestPlayer(player, putContext(params, CONTEXT_STATUS), getPlayerConnection(player));
			final Status status = parseStatus(statusJson);
			return status;
		}
		catch (IOException e) {
			playerConnectionPool.removePlayerConnection(player.getId());
			throw new PlayerConnectException("Connection of " + player.getName() + " is reset");
		}
	}

	@Override
	public PlayListDto requestPlaylist(Player player) throws PlayerConnectException {
		try {
			final String playlistJson = requestPlayer(player, putContext(new HashMap<String, Object>(), CONTEXT_PLAYLIST),  getPlayerConnection(player));
			final PlayListDto playlist = parsePlaylist(playlistJson);
			return playlist;
		}
		catch (IOException e) {
			playerConnectionPool.removePlayerConnection(player.getId());
			throw new PlayerConnectException("Connection of " + player.getName() + " is reset");
		}
	}

	@Override
	public List<MediaFile> requestBrowse(Player player, Map<String, Object> params) throws PlayerConnectException {
		try {
			final String filesJson = requestPlayer(player, putContext(params, CONTEXT_BROWSE),  getPlayerConnection(player));
			return parseMediaFiles(filesJson);
		}
		catch (IOException e) {
			playerConnectionPool.removePlayerConnection(player.getId());
			throw new PlayerConnectException(player.getName());
		}
	}

	private String toTcpParametter(Map<String, Object> params) {
		return gson.toJson(params);
	}

	private String requestPlayer(Player player, Map<String, Object> params, PlayerConnection connection) throws PlayerConnectException, IOException {
		if (connection == null) {
			throw new PlayerConnectException("Connection of " + player.getName() + " is reset");
		}
		return sendRequest(connection, toTcpParametter(params));
	}

	private String sendRequest(PlayerConnection connection, String tcpParametter) throws IOException {
		return connection.sendString(tcpParametter);
	}

	@Override
	public Schedule requestSchedule(Player player, Map<String, Object> params)
			throws PlayerConnectException {
		try {
			final String scheduleJson = requestPlayer(player, putContext(params, CONTEXT_SCHEDULE),  getPlayerConnection(player));
			return parseSchedule(scheduleJson);
		}
		catch (final IOException e) {
			playerConnectionPool.removePlayerConnection(player.getId());
			throw new PlayerConnectException("Connection of " + player.getName() + " is reset");
		}
	}

	@Override
	public boolean hasPlayerConnection(Player player) {
		return playerConnectionPool.hasPlayerConnection(player.getId());
	}
	

	private Status parseStatus(String statusJson) throws PlayerConnectException {
		final JSONObject jsonObj = analyzePlayerResponse(statusJson);
		try {
			return gson.fromJson(getObject(jsonObj), Status.class);
		}
		catch (JsonSyntaxException | JSONException e) {
			throw new PlayerConnectException(e.getMessage());
		}
	}

	private PlayListDto parsePlaylist(String playlistJson) throws PlayerConnectException {
		final JSONObject jsonObj = analyzePlayerResponse(playlistJson);
		try {
			return gson.fromJson(getObject(jsonObj), PlayListDto.class);
		}
		catch (JsonSyntaxException | JSONException e) {
			throw new PlayerConnectException(e.getMessage());
		}
	}

	public static JSONObject analyzePlayerResponse(String jsonString) throws PlayerConnectException {
		try {
			if (jsonString == null) {
				throw new JSONException("Player response is null");
			}
			final JSONObject jsonObject = new JSONObject(jsonString);
			final String result = (String) jsonObject.get("status");
			if (result.equals("error")) {
				handleResponseException(jsonObject);
			}
			return jsonObject;
		}
		catch (final JSONException e) {
			throw new PlayerConnectException(e.getMessage());
		}

	}

	public static void handleResponseException(JSONObject jsonObject) throws PlayerConnectException, JSONException {
		final String message = jsonObject.get("message").toString();
		final String clazz = jsonObject.get("clazz").toString();
		if (PlayerConnectException.class.getSimpleName().equals(clazz)) {
			throw new PlayerConnectException(message);
		}
	}
	

	private Schedule parseSchedule(String scheduleJson) throws PlayerConnectException {
		final JSONObject jsonObj = analyzePlayerResponse(scheduleJson);
		try {
			return gson.fromJson(getObject(jsonObj), Schedule.class);
		}
		catch (JsonSyntaxException | JSONException e) {
			throw new PlayerConnectException(e.getMessage());
		}
	}
	
	private List<MediaFile> parseMediaFiles(String filesJson) throws PlayerConnectException {
		final JSONObject jsonObject = analyzePlayerResponse(filesJson);
		try {
			return gson.fromJson(getObject(jsonObject), typeOfMediaFileList);
		}
		catch (JsonSyntaxException | JSONException e) {
			throw new PlayerConnectException(e.getMessage());
		}
	}

	public static String getObject(JSONObject jsonObject) throws JSONException {
		return (String) jsonObject.get("object");
	}

	@Override
	public PlayListDto updatePlaylist(Player player, Map<String, Object> params) throws PlayerConnectException {
		try {
			final String playlistJson = requestPlayer(player, putContext(params, CONTEXT_PLAYLIST),  getPlayerConnection(player));
			final PlayListDto pl = parsePlaylist(playlistJson);
			return pl;
		}
		catch (IOException e) {
			playerConnectionPool.removePlayerConnection(player.getId());
			throw new PlayerConnectException("Connection of " + player.getName() + " is reset");
		}
	}
}
