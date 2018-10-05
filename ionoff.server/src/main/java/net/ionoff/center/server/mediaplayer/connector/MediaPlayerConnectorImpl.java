package net.ionoff.center.server.mediaplayer.connector;

import static net.ionoff.center.server.mediaplayer.connector.MediaPlayerCommandBuilder.buildCommandGetStatus;

import com.google.gson.Gson;
import net.ionoff.center.server.broker.BrokerClientFactory;
import net.ionoff.center.server.broker.BrokerHttpClient;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerRequestException;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.mediaplayer.model.MediaPlayerResponse;
import net.ionoff.center.server.wsclient.RestTemplateFactory;
import net.ionoff.center.server.wsclient.RestTemplateRequestIntercepter;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.ScheduleDto;
import net.xapxinh.center.shared.dto.StatusDto;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MediaPlayerConnectorImpl implements IMediaPlayerConnector {

	private static final Logger LOGGER = Logger.getLogger(MediaPlayerConnectorImpl.class.getName());
	private static final Gson GSON = new Gson();

	// private final Type typeOfMediaFileList = new TypeToken<ArrayList<MediaFile>>(){}.getType();

	private final BrokerHttpClient brokerHttpClient;

    @Autowired
	public MediaPlayerConnectorImpl(BrokerClientFactory brokerClientFactory) {
    	RestTemplate restClient = RestTemplateFactory.buildRestTemplate(new MediaPlayerRequestExceptionHandler(),
			                    Arrays.asList(new RestTemplateRequestIntercepter()));
		brokerHttpClient = brokerClientFactory.createHttpClient(restClient);
	}

	private Map<String, Object> putContext(Map<String, Object> params, String context) {
		params.put("context", context);
		return params;
	}

	@Override
	public StatusDto requestStatus(MediaPlayer player, Map<String, Object> params) {
		return requestPlayer(player, putContext(params, CONTEXT_STATUS), StatusDto.class);
	}

	@Override
	public PlayListDto requestPlaylist(MediaPlayer player) {
		return requestPlayer(player, putContext(new HashMap<>(), CONTEXT_PLAYLIST), PlayListDto.class);
	}

	@Override
	public List<MediaFile> requestBrowse(MediaPlayer player, Map<String, Object> params) {
		return requestPlayer(player, putContext(params, CONTEXT_BROWSE), List.class);
	}

	@Override
	public ScheduleDto requestSchedule(MediaPlayer player, Map<String, Object> params) {
		return requestPlayer(player, putContext(params, CONTEXT_SCHEDULE), ScheduleDto.class);
	}

	@Override
	public PlayListDto updatePlaylist(MediaPlayer player, Map<String, Object> params) {
		return requestPlayer(player, putContext(params, CONTEXT_PLAYLIST), PlayListDto.class);
	}

	private <T> T requestPlayer(MediaPlayer player, Map<String, Object> params, Class<T> clazz) {
		if (!player.isOnline()) {
			throw new MediaPlayerConnectException(player.getName());
		}
    	try {
			BrokerResponse brokerResponse = brokerHttpClient.sendCommand(buildCommandGetStatus(player, params));
			MediaPlayerResponse playerResponse = extractDataFromResponse(brokerResponse);
			if (playerResponse.isError()) {
				throw handlePlayerResponseError(playerResponse);
			 } else {
				return extractObjectFromResponse(playerResponse, clazz);
			}
		} catch (MediaPlayerRequestException e) {
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
			throw new MediaPlayerRequestException(e.getClass().getSimpleName() + " " + e.getMessage());
		}
	}

	private static RuntimeException handlePlayerResponseError(MediaPlayerResponse playerResponse) {
		throw new MediaPlayerRequestException(playerResponse.getClazz() + " " + playerResponse.getMessage());
	}

	private static MediaPlayerResponse extractDataFromResponse(BrokerResponse brokerResponse) {
		try {
			String json = GSON.toJson(brokerResponse.getData());
			return GSON.fromJson(json, MediaPlayerResponse.class);
		} catch (Exception e) {
			LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
			throw new MediaPlayerRequestException(e.getClass().getSimpleName() + " Error extract from broker response", e);
		}
	}

	private static <T> T extractObjectFromResponse(MediaPlayerResponse playerResponse, Class<T> clazz) {
		try {
			String json = GSON.toJson(playerResponse.getObject());
			return GSON.fromJson(json, clazz);
		} catch (Exception e) {
			LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
			throw new MediaPlayerRequestException(e.getClass().getSimpleName() +  " Error extract from player response", e);
		}
	}

}
