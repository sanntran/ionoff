package net.ionoff.center.server.player.service;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.PlayList;
import net.ionoff.center.server.objmapper.PlayListMapper;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.server.persistence.service.IPlayNodeService;
import net.ionoff.center.server.player.api.ImpPlayerApi;
import net.xapxinh.center.server.api.data.DataServiceApi;
import net.xapxinh.center.server.api.player.IPlayerApi;
import net.xapxinh.center.server.entity.PlayLeaf;
import net.xapxinh.center.server.entity.PlayNode;
import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.service.player.AbstractPlayerConnectionPool;
import net.xapxinh.center.server.service.player.PlayerCaches;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.PlayLeafDto;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;

public class PlayerServiceImpl extends net.xapxinh.center.server.service.player.AbstractPlayerServiceImpl {

	@Autowired
	private ImpPlayerApi impPlayerApi;
	
	@Autowired
	private PlayListMapper playListMapper;
	
	@Autowired
	private IPlayListService playListService;
	
	@Autowired
	private IPlayNodeService playNodeService;
	
	public PlayerServiceImpl(IPlayerApi playerApi, AbstractPlayerConnectionPool playerConnectionPool, 
			PlayerCaches playerCaches, DataServiceApi dataServiceApi) {
		super(playerApi, playerConnectionPool, playerCaches, dataServiceApi);
	}
	
	@Override
	protected IPlayerApi getPlayerApi(Player player) {
		if (net.ionoff.center.server.entity.Player.IMP.equals(player.getModel())) {
			return impPlayerApi;
		}
		return super.getPlayerApi(player);
	}
	
	@Override
	protected void putAlbumParam(Player player, Map<String, Object> params, Album album) {
		if (net.ionoff.center.server.entity.Player.IMP.equals(player.getModel())) {
			params.put(INPUT, gson.toJson(album));
		}
		else {
			params.put(INPUT, gson.toJson(album));
		}
	}
	
	@Override
	protected String getActualYoutubeVideoUrl(Player player, String videoId) throws DataServiceException {
		if (net.ionoff.center.server.entity.Player.IMP.equals(player.getModel())) {
			return dataServiceApi.getYoutubeAudioUrl(player, videoId);
		}
		else {
			return dataServiceApi.getYoutubeVideoUrl(player, videoId);
		}
		
	}

	@Override
	protected PlayListDto getPlayList(Player player, Long playLisId) {
		PlayList playlist = playListService.findById(playLisId);
		return toPlayListDto(playlist);
	}

	private PlayListDto toPlayListDto(PlayList playlist) {
		PlayListDto playListDto = playListMapper.createPlayListDto(playlist);
		for (PlayNode node : playlist.getNodes()) {
			playListDto.getNodes().add(playListMapper.createPlayNodeDto(node));
		}
		return playListDto;
	}

	@Override
	protected PlayNodeDto getPlayNode(Player player, Long playNodeId) {
		PlayNode playnode = playNodeService.findById(playNodeId);
		return playListMapper.createPlayNodeDto(playnode);
	}

	@Override
	protected PlayLeafDto getPlayLeaf(Player player, Long playLeafId) {
		PlayLeaf playleaf = playNodeService.findLeafById(playLeafId);
		return playListMapper.createPlayLeafDto(playleaf);
	}
}
