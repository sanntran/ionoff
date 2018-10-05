package net.ionoff.center.server.controller;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.mediadata.exception.MediaDataConnectException;
import net.ionoff.center.server.mediadata.exception.MediaDataRequestException;
import net.ionoff.center.server.mediadata.service.IMediaDataService;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerRequestException;
import net.ionoff.center.server.mediaplayer.locale.PlayMessages;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.shared.dto.DeviceDto;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.MessageDto;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.ScheduleDto;
import net.xapxinh.center.shared.dto.StatusDto;
import net.xapxinh.center.shared.dto.YoutubeVideosDto;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RestController
@EnableWebMvc
public class MediaPlayerServiceController {

	private static final Logger LOGGER = Logger.getLogger(MediaPlayerServiceController.class.getName());

	@Autowired
	private IDeviceService deviceService;
	
	@Autowired
	private IPlayListService playListService;

	@Autowired
	private IMediaDataService dataService;

	@Autowired
	protected IMediaPlayerService playerService;


	protected MediaPlayer getPlayer(Long playerId) {
		return deviceService.getPlayer(playerId);
	}
	
	@RequestMapping(value = "playlists",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8",
			consumes = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public PlayListDto insertPlayList(
			@RequestParam("playerId") Long playerId, 
			@RequestBody PlayListDto playListDto, 
			HttpServletRequest request) {
		
		DeviceDto playerDto = deviceService.requireDtoById(playerId);
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, playerDto.getZoneId());
		
		LOGGER.info("User " + user.getName() + " inserts playlist: " + playListDto.toString());
		PlayListDto newPlayListDto = playListService.insertDto(user, playListDto);

		return playerService.updatePlaylist(getPlayer(playerId), newPlayListDto);
	}
	
	@RequestMapping(value = "playlists/{playlistId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public PlayListDto updatePlayList(
			@PathVariable("playlistId") Long playlistId, 
			@RequestParam("playerId") Long playerId, 
			@RequestBody PlayListDto playListDto, 
			HttpServletRequest request) {
		
		DeviceDto playerDto = deviceService.requireDtoById(playerId);
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, playerDto.getZoneId());
		
		LOGGER.info("User " + user.getName() + " updates playlist: " + playListDto.toString());
		PlayListDto updatedPlayListDto = playListService.updateDto(user, playListDto);
		
		return playerService.updatePlaylist(getPlayer(playerId), updatedPlayListDto);
	}

	@RequestMapping(value = "players/{playerId}/status",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public StatusDto getStatus(@PathVariable("playerId") Long playerId) {
		final MediaPlayer player = getPlayer(playerId);
		return playerService.requesStatus(player, new HashMap<String, Object>());
	}

	@RequestMapping(value = "players/{playerId}/playinglist",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public PlayListDto getPlaylist(@PathVariable("playerId") Long playerId) {
		final MediaPlayer player = getPlayer(playerId);
		return playerService.requestPlaylist(player);
	}

	@RequestMapping(value = "players/{playerId}/mediafiles",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<MediaFile> getMediaFiles(@PathVariable("playerId") Long playerId,
	                                     @RequestParam("dir") String dir) {
		final MediaPlayer player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("dir", dir);
		return playerService.requestMediaFiles(player, params);
	}

	@RequestMapping(value = "players/{playerId}/mediafiles/refresh",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<MediaFile> refreshMediaFiles(@PathVariable("playerId") Long playerId,
	                                         @RequestParam("dir") String dir) {
		final MediaPlayer player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("dir", dir);
		params.put("command", "refresh");
		return playerService.requestMediaFiles(player, params);
	}

	@RequestMapping(value = "players/{playerId}/scheduler",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public ScheduleDto getSchedule(@PathVariable("playerId") Long playerId) {
		final MediaPlayer player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("command", "get");
		return playerService.requestSchedule(player, params);
	}

	@RequestMapping(value = "players/{playerId}/scheduler",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	public ScheduleDto getSchedule(@PathVariable("playerId") Long playerId, @RequestBody ScheduleDto schedule) {
		final MediaPlayer player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("command", "update");
		params.put("action", schedule.getAction());
		params.put("dateTime", schedule.getDateTime());
		return playerService.requestSchedule(player, params);
	}

	@RequestMapping(value = "players/{playerId}/command",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	public StatusDto sendCommand(@PathVariable("playerId") Long playerId, @RequestBody Command command) {
		final MediaPlayer player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("command", command.getCommand());
		params.put("id", command.getId());
		params.put("val", command.getVal());
		params.put("input", command.getInput());
		params.put("input_type", command.getInput_type());
		params.put("type", command.getType());
		params.put("title", command.getTitle());
		return playerService.requesStatus(player, params);
	}

	@RequestMapping(value = "youtubevideos/search",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public YoutubeVideosDto searchYoutubeVideos(@RequestParam("playerId") Long playerId,
	                                            @RequestParam("searchKey") String searchKey,
	                                            @RequestParam("pageToken") String pageToken) throws IOException {
		final MediaPlayer player = getPlayer(playerId);
		return dataService.searchYoutubeVideos(player, searchKey, pageToken);
	}

	@RequestMapping(value = "albums/search",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<Album> searchAlbums(@RequestParam("playerId") Long playerId,
	                                @RequestParam("searchKey") String searchKey,
	                                @RequestParam("searchScope") String searchScope,
	                                @RequestParam("pageNumber") Integer pageNumber,
	                                @RequestParam("pageSize") Integer pageSize) {

		final MediaPlayer player = getPlayer(playerId);
		final Map<String, String> params = new HashMap<String, String>();
		params.put("searchKey", searchKey);
		params.put("searchScope", searchScope);
		params.put("pageNumber", pageNumber + "");
		params.put("pageSize", pageSize + "");
		return dataService.searchAlbums(player, searchKey, searchScope, pageNumber, pageSize);
	}

	@RequestMapping(value = "albums/{albumId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public Album getAlbum(@PathVariable("albumId") Long albumId,
	                      @RequestParam("playerId") Long playerId) {
		final MediaPlayer player = getPlayer(playerId);
		return dataService.getAlbum(player, albumId);
	}

	@RequestMapping(value = "albums/special",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<Album> getSpecialAlbum(@RequestParam("playerId") Long playerId) {
		final MediaPlayer player = getPlayer(playerId);
		return dataService.getSpecialAlbums(player);
	}

	@ExceptionHandler(MediaPlayerRequestException.class)
	@ResponseBody
	public MessageDto handleException(HttpServletRequest request, HttpServletResponse response, MediaPlayerRequestException e) {
		final String locale = (String) request.getAttribute("locale");
		if (e instanceof MediaPlayerConnectException) {
			LOGGER.error("MediaPlayerConnectException " + e.getMessage());
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					PlayMessages.get(locale).playerConnectionError());
		} else {
			LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					e.getMessage());
		}
	}

	@ExceptionHandler(MediaDataRequestException.class)
	@ResponseBody
	public MessageDto handleException(HttpServletRequest request, HttpServletResponse response, MediaDataRequestException e) {
		final String locale = (String) request.getAttribute("locale");
		if (e instanceof MediaDataConnectException) {
			LOGGER.error("MediaDataConnectException " + e.getMessage(), e);
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					PlayMessages.get(locale).dataServiceConnecttionError());
		}
		else {
			LOGGER.error(e.getMessage());
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					e.getMessage());
		}
	}
}
