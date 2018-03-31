package net.xapxinh.center.server.webmvc;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import net.xapxinh.center.server.api.data.DataServiceConnectException;
import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.exception.PlayServerException;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.server.exception.UnknownPlayerException;
import net.xapxinh.center.server.locale.PlayMessages;
import net.xapxinh.center.server.service.data.DataService;
import net.xapxinh.center.server.service.player.IPlayerService;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.Schedule;
import net.xapxinh.center.shared.dto.Status;
import net.xapxinh.center.shared.dto.YoutubeVideos;

public abstract class AbstractPlayerServiceController {

	private static final  Logger LOGGER = Logger.getLogger(AbstractPlayerServiceController.class.getName());

	@Autowired
	private DataService dataService;
	@Autowired
	protected IPlayerService playerService;

	protected abstract Player getPlayer(Long playerId) throws UnknownPlayerException;

	@RequestMapping(value = "players/{playerId}/status",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Status getStatus(@PathVariable("playerId") Long playerId) throws UnknownPlayerException, PlayerConnectException, DataServiceException {
		final Player player = getPlayer(playerId);
		return playerService.requesStatus(player, new HashMap<String, Object>()); 
	}

	@RequestMapping(value = "players/{playerId}/playinglist",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public PlayListDto getPlaylist(@PathVariable("playerId") Long playerId
			) throws PlayerConnectException, UnknownPlayerException {
		final Player player = getPlayer(playerId);
		return playerService.requestPlaylist(player);
	}

	@RequestMapping(value = "players/{playerId}/mediafiles",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<MediaFile> getMediaFiles(@PathVariable("playerId") Long playerId,
			@RequestParam("dir") String dir)
					throws PlayerConnectException, UnknownPlayerException {
		final Player player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("dir", dir);
		return playerService.requestMediaFiles(player, params);
	}
	
	@RequestMapping(value = "players/{playerId}/mediafiles/refresh",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<MediaFile> refreshMediaFiles(@PathVariable("playerId") Long playerId,
			@RequestParam("dir") String dir)
					throws PlayerConnectException, UnknownPlayerException {
		final Player player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("dir", dir);
		params.put("command", "refresh");
		return playerService.requestMediaFiles(player, params);
	}
	
	@RequestMapping(value = "players/{playerId}/schedule",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public Schedule getSchedule(@PathVariable("playerId") Long playerId)
			throws PlayerConnectException, UnknownPlayerException {
		final Player player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("command", "get");
		return playerService.requestSchedule(player, params);
	}

	@RequestMapping(value = "players/{playerId}/schedule",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	public Schedule getSchedule(@PathVariable("playerId") Long playerId, @RequestBody Schedule schedule)
			throws PlayerConnectException, UnknownPlayerException {
		final Player player = getPlayer(playerId);
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("command", "update");
		params.put("action", schedule.getAction());
		params.put("dateTime", schedule.getDateTime());
		return playerService.requestSchedule(player, params);
	}

	@RequestMapping(value = "players/{playerId}/command",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	public Status sendCommand(@PathVariable("playerId") Long playerId, @RequestBody Command command)
			throws PlayerConnectException, UnknownPlayerException, DataServiceException {
		final Player player = getPlayer(playerId);
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
	public YoutubeVideos searchYoutubeVideos(@RequestParam("playerId") Long playerId,
			@RequestParam("searchKey") String searchKey,
			@RequestParam("pageToken") String pageToken)
			throws PlayerConnectException, UnknownPlayerException, IOException {
		final Player player = getPlayer(playerId);
		
		return dataService.searchYoutubeVideos(player, searchKey, pageToken);
	}

	@RequestMapping(value = "albums/search",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<Album> searchAlbums(@RequestParam("playerId") Long playerId,
			@RequestParam("searchKey") String searchKey,
			@RequestParam("searchScope") String searchScope,
			@RequestParam("pageNumber") Integer pageNumber,
			@RequestParam("pageSize") Integer pageSize)
			throws PlayerConnectException, UnknownPlayerException, DataServiceException {

		final Player player = getPlayer(playerId);
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
			@RequestParam("playerId") Long playerId)
			throws PlayerConnectException, UnknownPlayerException, DataServiceException {
		final Player player = getPlayer(playerId);
		return dataService.getAlbum(player, albumId);
	}

	@RequestMapping(value = "albums/special",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	public List<Album> getSpecialAlbum(@RequestParam("playerId") Long playerId)
			throws PlayerConnectException, UnknownPlayerException, DataServiceException {
		final Player player = getPlayer(playerId);
		return dataService.getSpecialAlbums(player);
	}

	@ExceptionHandler(PlayServerException.class)
	@ResponseBody
	public ServerMessageResponse handleException(HttpServletRequest request, HttpServletResponse response, PlayServerException e) {
		final String locale = (String) request.getAttribute("locale");
		if (e instanceof PlayerConnectException) {
			if (e.getMessage() == null || e.getMessage().isEmpty()) {
				LOGGER.error("PlayerConnectException " + e.getMessage(), e);
			}
			else {
				LOGGER.error("PlayerConnectException " + e.getMessage());
			}
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new ServerMessageResponse(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					PlayMessages.get(locale).playerConnectionError());
		}
		else if (e instanceof UnknownPlayerException) {
			LOGGER.error(e.getMessage() + " is unknown");
			response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			return new ServerMessageResponse(HttpServletResponse.SC_BAD_REQUEST,
					PlayMessages.get(locale).playerIsUnknown(e.getMessage()));
		}
		else {
			LOGGER.error(e.getMessage(), e);
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new ServerMessageResponse(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					e.getMessage());
		}
	}

	@ExceptionHandler(DataServiceException.class)
	@ResponseBody
	public ServerMessageResponse handleException(HttpServletRequest request, HttpServletResponse response, DataServiceException e) {
		final String locale = (String) request.getAttribute("locale");
		if (e instanceof DataServiceConnectException) {
			LOGGER.error(e.getMessage(), e);
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new ServerMessageResponse(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					PlayMessages.get(locale).dataServiceConnecttionError());
		}
		else {
			LOGGER.error(e.getMessage());
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			return new ServerMessageResponse(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
					e.getMessage());
		}
	}
}
