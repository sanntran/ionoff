package net.xapxinh.center.server.entity;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

public class PlayNode implements Serializable {

	private static final long serialVersionUID = 1L;
	
	public enum TYPE {
		album, dir, youtube;
	}
	
	private long id;
	private String name;
	private Integer idx;
	private String type;
	private String image;
	private List<PlayLeaf> leafs;
	private PlayList playList;
	
	public long getId() {
		return id;
	}
	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	public Integer getIdx() {
		return idx;
	}

	public void setIdx(Integer idx) {
		this.idx = idx;
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

	public PlayList getPlayList() {
		return playList;
	}

	public void setPlayList(PlayList playList) {
		this.playList = playList;
	}

	public List<PlayLeaf> getLeafs() {
		if (leafs == null) {
			return Collections.emptyList();
		}
		return leafs;
	}

	public void setLeafs(List<PlayLeaf> playlistLeafs) {
		this.leafs = playlistLeafs;
	}

	public boolean hasLeaf() {
		return leafs != null && !leafs.isEmpty();
	}

	public boolean containsLeaf(PlayLeaf leaf) {
		if (!hasLeaf()) {
			return false;
		}
		return leafs.contains(leaf);
	}
	
	public boolean isEmpty() {
		return getLeafs().isEmpty();
	}

	public boolean hasManyLeaf() {
		return getLeafs().size() > 1;
	}

	public boolean hasLeaf(long leafId) {
		for (PlayLeaf leaf : getLeafs()) {
			if (leafId == leaf.getId()) {
				return true;
			}
		}
		return false;
	}

	public PlayLeaf getLeaf(long leafId) {
		for (PlayLeaf leaf : getLeafs()) {
			if (leafId == leaf.getId()) {
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
