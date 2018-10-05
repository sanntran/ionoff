package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.PlayList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.mapper.PlayListMapper;
import net.ionoff.center.server.persistence.dao.IPlayLeafDao;
import net.ionoff.center.server.persistence.dao.IPlayNodeDao;
import net.ionoff.center.server.persistence.service.IPlayListService;
import net.ionoff.center.server.persistence.service.IPlayNodeService;
import net.ionoff.center.server.entity.PlayLeaf;
import net.ionoff.center.server.entity.PlayNode;
import net.xapxinh.center.shared.dto.PlayLeafDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;

@Transactional
public class PlayNodeServiceImpl extends AbstractGenericService<PlayNode, PlayNodeDto> implements IPlayNodeService {

	private IPlayNodeDao playNodeDao;
	
	@Autowired
	private IPlayLeafDao playLeafDao;
	
	@Autowired
	private IPlayListService playListService;
	
	@Autowired
	private PlayListMapper playListMapper;
	
	public PlayNodeServiceImpl(IPlayNodeDao playNodeDao) {
		this.playNodeDao = playNodeDao;
	}

	@Override
	protected IPlayNodeDao getDao() {
		return playNodeDao;
	}
	
	@Override
	public PlayNode insert(PlayNode playNode) {
		playNodeDao.insert(playNode);
		for (PlayLeaf leaf : playNode.getLeafs()) {
			playLeafDao.insert(leaf);
		}
		return playNode;
	}
	
	@Override
	public PlayNode update(PlayNode playNode) {
		playNodeDao.update(playNode);
		
		List<PlayLeaf> playLeafs = playLeafDao.findByNodeId(playNode.getId());
		
		for (PlayLeaf leaf : playLeafs) {
			boolean isRemoved = true;
			for (PlayLeaf newLeaf : playNode.getLeafs()) {
				if (newLeaf.getId() == leaf.getId()) {
					isRemoved = false;
					break;
				}
			}
			if (isRemoved) {
				playLeafDao.delete(leaf);
			}
		}
		
		for (PlayLeaf newLeaf : playNode.getLeafs()) {
			boolean isAdded = true;
			for (PlayLeaf leaf : playLeafs) {
				if (newLeaf.getId() == leaf.getId()) {
					isAdded = false;
					break;
				}
			}
			if (isAdded) {
				playLeafDao.insert(newLeaf);
			}
			else {
				playLeafDao.update(newLeaf);
			}
		}
			
		return playNode;
	}
	
	@Override
	public List<PlayNode> findByPlayListId(long playListId) {
		return playNodeDao.findByPlayListId(playListId);
	}

	@Override
	public PlayLeaf findLeafById(long leafId) {
		return playLeafDao.findById(leafId);
	}

	@Override
	public PlayNodeDto insertDto(User user, PlayNodeDto dto) {
		PlayList playList = playListService.requireById(dto.getPlayListId());
		PlayNode playNode = new PlayNode();
		playNode.setPlayList(playList);
		playListMapper.updatePlayNode(playNode, dto);
		insert(playNode);
		return playListMapper.createPlayNodeDto(playNode);
	}

	@Override
	public PlayNodeDto updateDto(User user, PlayNodeDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public PlayNodeDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<PlayNodeDto> findDtoByPlayListId(User user, Long playlistId) {
		PlayList playlist = playListService.requireById(playlistId);
		if (playlist.getUser().getId() != user.getId()) {
			throw new AccessDeniedException("Access denied. (User: " + user.getName() + ", Playlist ID: " + playlistId + ")");
		}
		List<PlayNodeDto> nodeDtos = new ArrayList<>();
		for (int i = 0 ; i < playlist.getNodes().size(); i++) {
			nodeDtos.add(playListMapper.createPlayNodeDto(playlist.getNodes().get(i)));
		}
		return nodeDtos;
	}
	
	@Override
	public PlayNodeDto findNodeDtoById(Long playNodeId) {
		return playListMapper.createPlayNodeDto(findById(playNodeId));
	}

	@Override
	public PlayLeafDto findLeafDtoById(Long playLeafId) {
		return playListMapper.createPlayLeafDto(playLeafDao.findById(playLeafId));
	}

	@Override
	public void deleteDtoById(User user, long id) {
		PlayNode playNode = requireById(id);
		delete(playNode);
	}

	@Override
	protected List<PlayNodeDto> createDtoList(List<PlayNode> entities) {
		throw new UnsupportedOperationException();
	}
}
