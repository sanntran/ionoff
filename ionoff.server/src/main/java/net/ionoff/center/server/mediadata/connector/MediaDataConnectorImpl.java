package net.ionoff.center.server.mediadata.connector;

import net.ionoff.center.server.mediadata.exception.MediaDataRequestException;
import net.ionoff.center.server.mediadata.model.AlbumArrayList;
import net.ionoff.center.server.mediadata.model.MediaDataErrorMesage;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.util.AccentRemover;
import net.ionoff.center.server.wsclient.RestTemplateFactory;
import net.ionoff.center.server.wsclient.RestTemplateRequestIntercepter;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.SongDto;
import net.xapxinh.center.shared.dto.YoutubeVideosDto;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.List;

public class MediaDataConnectorImpl implements IMediaDataConnector {


	private final Logger logger = Logger.getLogger(MediaDataConnectorImpl.class.getName());
	@Value("${service.media.url}")
	private String mediaServiceUrl;

	@Override
	public String getDataServiceUrl() {
		return mediaServiceUrl;
	}

	private final RestTemplate restClient;

	public MediaDataConnectorImpl() {
		restClient = RestTemplateFactory.buildRestTemplate(new MediaDataRequestExceptionHandler(),
				Arrays.asList(new RestTemplateRequestIntercepter()));
	}

	@Override
	public List<Album> searchAlbums(MediaPlayer player, String searchKey, String searchScope, int pageNumber, int pageSize) {
		try {
			final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
					.path("/albums/search")
					.queryParam("mac", player.getMac())
					.queryParam("searchKey", URLEncoder.encode(AccentRemover.removeAccent(searchKey), "UTF-8"))
					.queryParam("searchScope", searchScope)
					.queryParam("pageNumber", String.valueOf(pageNumber))
					.queryParam("pageSize", String.valueOf(pageSize))
					.build()
					.toUri();
			return restClient.getForObject(targetUrl, AlbumArrayList.class);
		} catch (UnsupportedEncodingException e) {
			logger.error("UnsupportedEncodingException " + e.getMessage());
			throw new MediaDataRequestException("UnsupportedEncodingException " + e.getMessage());
		}
	}

	@Override
	public void insertPlayer(MediaPlayer player) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/players")
				.build()
				.toUri();
		restClient.put(targetUrl, player.getMac());
	}

	@Override
	public Album getAlbum(MediaPlayer player, Long albumId) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/albums/" + albumId) // /albums/{albumId}
				.queryParam("mac", player.getMac())
				.build()
				.toUri();
		return restClient.getForObject(targetUrl.toString(), Album.class);
	}

	@Override
	public List<Album> getSpecialAlbums(MediaPlayer player) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/albums/special")
				.queryParam("mac", player.getMac())
				.build()
				.toUri();
		return restClient.getForObject(targetUrl, AlbumArrayList.class);
	}


	@Override
	public void increaseAlbumListenCount(MediaPlayer player, Long albumId) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/albums/" + albumId) // /albums/{albumId}
				.queryParam("mac", player.getMac())
				.build()
				.toUri();
		restClient.postForObject(targetUrl.toString(), albumId, MediaDataErrorMesage.class);
	}


	@Override
	public YoutubeVideosDto searchYoutubeVideos(MediaPlayer player, String key, String pageToken) {
		try {
			final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
                    .path("/videos/search")
                    .queryParam("mac", player.getMac())
                    .queryParam("key", URLEncoder.encode(AccentRemover.removeAccent(key), "UTF-8"))
                    .queryParam("pageToken", pageToken)
                    .build()
                    .toUri();
			return restClient.getForObject(targetUrl, YoutubeVideosDto.class);
		} catch (UnsupportedEncodingException e) {
			logger.error("UnsupportedEncodingException " + e.getMessage());
			throw new MediaDataRequestException("UnsupportedEncodingException " + e.getMessage());
		}
	}


	@Override
	public String getYoutubeVideoUrl(MediaPlayer player, String videoId) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/videos/" + videoId + "/videourl")
				.queryParam("mac", player.getMac())
				.build()
				.toUri();

		return restClient.getForObject(targetUrl, String.class);
	}

	@Override
	public String getYoutubeAudioUrl(MediaPlayer player, String videoId) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/videos/" + videoId + "/audiourl")
				.queryParam("mac", player.getMac())
				.build()
				.toUri();

		return restClient.getForObject(targetUrl, String.class);
	}


	@Override
	public SongDto getSong(MediaPlayer player, Long songId) {
		final URI targetUrl = UriComponentsBuilder.fromUriString(getDataServiceUrl())
				.path("/songs/" + songId) // /songs/{albumId}
				.queryParam("mac", player.getMac())
				.build()
				.toUri();
		return restClient.getForObject(targetUrl.toString(), SongDto.class);
	}

}
