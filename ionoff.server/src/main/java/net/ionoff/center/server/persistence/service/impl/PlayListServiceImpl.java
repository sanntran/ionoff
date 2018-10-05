package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.PlayList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.mapper.PlayListMapper;
import net.ionoff.center.server.persistence.dao.IPlayListDao;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.server.persistence.service.IPlayNodeService;
import net.ionoff.center.server.entity.PlayNode;
import net.xapxinh.center.shared.dto.PlayListDto;

@Transactional
public class PlayListServiceImpl extends AbstractGenericService<PlayList, PlayListDto> implements IPlayListService {

	@Autowired
	private IPlayNodeService playNodeService;
	
	@Autowired
	private PlayListMapper playListMapper;
	
	private IPlayListDao playListDao;
	
	public PlayListServiceImpl(IPlayListDao playListDao) {
		this.playListDao = playListDao;
	}

	@Override
	protected IPlayListDao getDao() {
		return playListDao;
	}
	
	
	@Override
	public PlayList insert(PlayList playList) {
		playListDao.insert(playList);
		List<PlayNode> playNodes = playList.getNodes();
		for (PlayNode node : playNodes) {
			playNodeService.insert(node);
		}
		return playList;
	}
	
	@Override
	public PlayList update(PlayList playList) {
		

		List<PlayNode> playNodes = playNodeService.findByPlayListId(playList.getId());
		
		for (PlayNode node : playNodes) {
			boolean isRemoved = true;
			for (PlayNode newNode : playList.getNodes()) {
				if (newNode.getId() == node.getId()) {
					isRemoved = false;
					break;
				}
			}
			if (isRemoved) {
				playNodeService.delete(node);
			}
		}
		
		for (PlayNode newNode : playList.getNodes()) {
			boolean isAdded = true;
			for (PlayNode node : playNodes) {
				if (newNode.getId() == node.getId()) {
					isAdded = false;
					break;
				}
			}
			if (isAdded) {
				playNodeService.insert(newNode);
			}
			else {
				playNodeService.update(newNode);
			}
		}
		
		playListDao.update(playList);
			
		return playList;
	}

	@Override
	public List<PlayListDto> findDtoByUser(User user) {
		List<PlayList> playLists = playListDao.findByUserId(user.getId());
		List<PlayListDto> playListDtos = new ArrayList<>();
		for (PlayList playList : playLists) {
			playListDtos.add(playListMapper.createPlayListDto(playList));
		}
		return playListDtos;
	}

	@Override
	public PlayListDto insertDto(User user, PlayListDto dto) {
		PlayList playlist = new PlayList();
		playlist.setUser(user);
		playListMapper.updatePlayList(playlist, dto);
		insert(playlist);
		dto.setId(playlist.getId());
		for (int i = 0 ; i < playlist.getNodes().size(); i++) {
			dto.getNodes().get(i).setId(playlist.getNodes().get(i).getId());
			for (int j = 0; j < playlist.getNodes().get(i).getLeafs().size(); j++) {
				dto.getNodes().get(i).getLeafs().get(j).setId(playlist.getNodes().get(i).getLeafs().get(j).getId());
			}
		}
		return dto;
	}

	@Override
	public PlayListDto updateDto(User user, PlayListDto dto) {
		PlayList playlist = findById(dto.getId());
		if (playlist == null) {
			return insertDto(user, dto);
		}
		playListMapper.updatePlayList(playlist, dto);
		update(playlist);
		dto.setId(playlist.getId());
		for (int i = 0 ; i < playlist.getNodes().size(); i++) {
			dto.getNodes().get(i).setId(playlist.getNodes().get(i).getId());
			for (int j = 0; j < playlist.getNodes().get(i).getLeafs().size(); j++) {
				dto.getNodes().get(i).getLeafs().get(j).setId(playlist.getNodes().get(i).getLeafs().get(j).getId());
			}
		}
		return dto;
	}

	@Override
	public PlayListDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public void deleteDtoById(User user, long playlistId) {
		PlayList playlist = requireById(playlistId);
		if (playlist.getUser().getId() != user.getId()) {
			throw new AccessDeniedException("Access denied. (User: " + user.getName() + ", Playlist ID: " + playlistId + ")");
		}
		delete(playlist);
	}

	@Override
	protected List<PlayListDto> createDtoList(List<PlayList> entities) {
		throw new UnsupportedOperationException();
	}
	
}
