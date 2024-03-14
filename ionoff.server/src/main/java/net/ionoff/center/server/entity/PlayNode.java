package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class PlayNode implements IEntity {

	private static final long serialVersionUID = 1L;
	
	public enum TYPE {
		album, dir, youtube;
	}

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Integer idx;
	private String type;
	private String image;
	private List<PlayLeaf> leafs;
	private PlayList playList;

	public List<PlayLeaf> getLeafs() {
		if (leafs == null) {
			return Collections.emptyList();
		}
		return leafs;
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

}
