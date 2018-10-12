package net.ionoff.center.server.controller;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.server.persistence.service.IPlayNodeService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.player.PlayListDto;
import net.ionoff.center.shared.dto.player.PlayNodeDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class PlayListServiceController {

	private final Logger logger = LoggerFactory.getLogger(PlayListServiceController.class.getName());
	
	@Autowired
	private IPlayListService playListService;
	
	@Autowired
	private IPlayNodeService playNodeService;
	
	@RequestMapping(value = "playlists",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<PlayListDto> getPlayLists(@RequestParam("playerId") Long playerId) {
		User user = RequestContextHolder.getUser();
		List<PlayListDto> playListDtos = playListService.findDtoByUser(user);
		return playListDtos;
	}
	
	@RequestMapping(value = "playlists/{playlistId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto deletePlayList(@PathVariable("playlistId") Long playlistId,
	                                 @RequestParam("playerId") Long playerId) {
		
		User user = RequestContextHolder.getUser();
		playListService.deleteDtoById(user, playlistId);
		return MessageDto.success(playlistId);
	}
	
	@RequestMapping(value = "playlists/{playlistId}/playnodes",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<PlayNodeDto> getPlayNodesByPlaylist(@PathVariable("playlistId") Long playlistId,
													@RequestParam("playerId") Long playerId) {
		
		User user = RequestContextHolder.getUser();
		
		return playNodeService.findDtoByPlayListId(user, playlistId);
	}
	
	@RequestMapping(value = "playnodes",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public PlayNodeDto insertPlayNode(@PathVariable("playlistId") Long playlistId,
											@RequestBody PlayNodeDto playNodeDto) {
		
		User user = RequestContextHolder.getUser();
		playNodeDto.setPlayListId(playlistId);
		logger.info("User " + user.getName() + " inserts playnode: " + playNodeDto.toString());
		return playNodeService.insertDto(user, playNodeDto);
	}

	@RequestMapping(value = "playnodes/{playnodeId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto deletePlayNode(@PathVariable("playnodeId") Long playnodeId) {
		
		User user = RequestContextHolder.getUser();
		logger.info("User " + user.getName() + " delete playnode, ID: " + playnodeId);
		playNodeService.deleteDtoById(user, playnodeId);
		return MessageDto.success(playnodeId);
	}

}
