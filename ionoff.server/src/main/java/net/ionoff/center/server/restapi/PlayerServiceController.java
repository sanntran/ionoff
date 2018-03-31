package net.ionoff.center.server.restapi;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.shared.dto.DeviceDto;
import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.PlayerConnectException;
import net.xapxinh.center.server.exception.UnknownPlayerException;
import net.xapxinh.center.server.webmvc.AbstractPlayerServiceController;
import net.xapxinh.center.shared.dto.PlayListDto;

@RestController
@EnableWebMvc
public class PlayerServiceController extends AbstractPlayerServiceController {

	private final Logger logger = Logger.getLogger(PlayerServiceController.class.getName());

	@Autowired
	private IDeviceService deviceService;
	
	@Autowired
	private IPlayListService playListService;
	
	@Override
	protected Player getPlayer(Long playerId) throws UnknownPlayerException {
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
			HttpServletRequest request) throws UnknownPlayerException, PlayerConnectException {
		
		DeviceDto playerDto = deviceService.requireDtoById(playerId);
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, playerDto.getZoneId());
		
		logger.info("User " + user.getName() + " inserts playlist: " + playListDto.toString());
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
			HttpServletRequest request) throws PlayerConnectException, UnknownPlayerException {
		
		DeviceDto playerDto = deviceService.requireDtoById(playerId);
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, playerDto.getZoneId());
		
		logger.info("User " + user.getName() + " updates playlist: " + playListDto.toString());
		PlayListDto updatedPlayListDto = playListService.updateDto(user, playListDto);
		
		return playerService.updatePlaylist(getPlayer(playerId), updatedPlayListDto);
	}
}
