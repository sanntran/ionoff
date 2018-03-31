package net.xapxinh.center.shared.dto;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

public class PlayNodeDto implements Serializable {

	private static final long serialVersionUID = 1L;
	
	public enum TYPE {
		album, dir, youtube;
	}
	private long id;
	private long idx;
	private String name;
	private String type;
	private String image;
	private Long playListId;
	private List<PlayLeafDto> leafs;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}
		
	public long getIdx() {
		return idx;
	}

	public void setIdx(long idx) {
		this.idx = idx;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getImage() {
		return image;
	}

	public void setImage(String image) {
		this.image = image;
	}

	public List<PlayLeafDto> getLeafs() {
		if (leafs == null) {
			return Collections.emptyList();
		}
		return leafs;
	}

	public void setLeafs(List<PlayLeafDto> playlistLeafs) {
		this.leafs = playlistLeafs;
	}

	public Long getPlayListId() {
		return playListId;
	}

	public void setPlayListId(Long playListId) {
		this.playListId = playListId;
	}

	public boolean hasLeaf() {
		return leafs != null && !leafs.isEmpty();
	}
	
	public boolean hasLeaf(PlayLeafDto leaf) {
		if (!hasLeaf()) {
			return false;
		}
		return leafs.contains(leaf);
	}

	public boolean isCurrent() {
		if (!hasLeaf()) {
			return false;
		}
		for (PlayLeafDto leaf : leafs) {
			if (leaf.isCurrent()) {
				return true;
			}
		}
		return false;
	}
	
	public boolean isEmpty() {
		return getLeafs().isEmpty();
	}

	public boolean hasManyLeaf() {
		return getLeafs().size() > 1;
	}

	public boolean hasLeaf(long leafIdx) {
		for (PlayLeafDto leaf : getLeafs()) {
			if (leafIdx == leaf.getIdx()) {
				return true;
			}
		}
		return false;
	}

	public PlayLeafDto findLeaf(long leafIdx) {
		for (PlayLeafDto leaf : getLeafs()) {
			if (leafIdx == leaf.getIdx()) {
				return leaf;
			}
		}
		return null;
	}

	public boolean isAlbum() {
		return type != null && TYPE.album.toString().endsWith(type);
	}

	public boolean isNew() {
		return id == 0L;
	}
}
