package net.ionoff.center.server.objmapper;

import java.util.ArrayList;

import net.ionoff.center.server.entity.PlayList;
import net.xapxinh.center.shared.dto.PlayLeafDto;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.persistence.service.IPlayNodeService;
import net.ionoff.center.server.entity.PlayLeaf;
import net.ionoff.center.server.entity.PlayNode;

public class PlayListMapper {

	@Autowired
	private IPlayNodeService playNodeService;
	
	public PlayListDto createPlayListDto(PlayList playList) {
		final PlayListDto playListDto = new PlayListDto();
		playListDto.setId(playList.getId());
		playListDto.setName(playList.getName());
		playListDto.setIsPublic(playList.getIsPublic());
		playListDto.setThumbnail(playList.getThumbnail());
		playListDto.setNodes(new ArrayList<>());
		return playListDto;
	}

	public PlayNodeDto createPlayNodeDto(PlayNode playNode) {
		final PlayNodeDto playNodeDto
					= new PlayNodeDto();
		playNodeDto.setId(playNode.getId());
		playNodeDto.setName(playNode.getName());
		playNodeDto.setType(playNode.getType());
		playNodeDto.setImage(playNode.getImage());
		playNodeDto.setLeafs(new ArrayList<>());
		for (PlayLeaf leaf : playNode.getLeafs()) {
			playNodeDto.getLeafs().add(createPlayLeafDto(leaf));
		}
		return playNodeDto;
	}
	
	public PlayLeafDto createPlayLeafDto(PlayLeaf playLeaf) {
		final PlayLeafDto playLeafDto = new PlayLeafDto();
		playLeafDto.setId(playLeaf.getId());
		playLeafDto.setName(playLeaf.getName());
		playLeafDto.setType(playLeaf.getType());
		playLeafDto.setUrl(playLeaf.getUrl());
		playLeafDto.setArtists(playLeaf.getArtists());
		playLeafDto.setAuthor(playLeaf.getAuthors());
		playLeafDto.setDuration(playLeaf.getDuration());
		playLeafDto.setMrl(playLeaf.getMrl());
		playLeafDto.setImage(playLeaf.getImage());
		return playLeafDto;
	}
	

	public void updatePlayList(PlayList playlist, PlayListDto playListDto) {
		playlist.setName(playListDto.getName());
		playlist.setThumbnail(playListDto.getThumbnail());
		playlist.setIsPublic(playListDto.getIsPublic());
		playlist.setNodes(new ArrayList<>());
		int idx = 0;
		for (PlayNodeDto nodeDto : playListDto.getNodes()) {
			PlayNode node = null;
			if (nodeDto.isNew()) {
				node = new PlayNode();
			}
			else {
				node = playNodeService.findById(nodeDto.getId());
			}
			if (node == null) {
				node = new PlayNode();
			}
			node.setIdx(idx);
			node.setPlayList(playlist);
			updatePlayNode(node, nodeDto);
			playlist.getNodes().add(node);
			idx ++;
		}
	}
	
	public void updatePlayNode(PlayNode playNode, PlayNodeDto playNodeDto) {
		playNode.setId(playNodeDto.getId());
		playNode.setName(playNodeDto.getName());
		playNode.setType(playNodeDto.getType());
		playNode.setLeafs(new ArrayList<>());
		int idx = 0;
		for (PlayLeafDto leafDto : playNodeDto.getLeafs()) {
			PlayLeaf leaf = null;
			if (leafDto.isNew()) {
				leaf = new PlayLeaf();
			}
			else {
				leaf = playNodeService.findLeafById(leafDto.getId());
			}
			if (leaf == null) {
				leaf = new PlayLeaf();
			}
			leaf.setIdx(idx);
			leaf.setId(leafDto.getId());
			leaf.setName(leafDto.getName());
			leaf.setImage(leafDto.getImage());
			leaf.setUrl(leafDto.getUrl());
			if (PlayLeaf.TYPE.youtube.toString().equals(leafDto.getType())) {
				leaf.setMrl(null);
			}
			else {
				leaf.setMrl(leafDto.getMrl());
			}
			leaf.setType(leafDto.getType());
			leaf.setArtists(leafDto.getArtists());
			leaf.setAuthors(leafDto.getAuthor());
			leaf.setDuration(leafDto.getDuration());
			leaf.setPlayNode(playNode);
			
			playNode.getLeafs().add(leaf);
			idx ++;
		}
	}

}
