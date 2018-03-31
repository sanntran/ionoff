package net.xapxinh.center.server.entity;

import java.io.Serializable;
import java.util.List;

public class PlayList implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private long id;
	private String name;
	private Boolean isPublic;
	private String thumbnail;
	private List<PlayNode> nodes;
	
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

	public Boolean getIsPublic() {
		return isPublic;
	}
	public void setIsPublic(Boolean isPublic) {
		this.isPublic = isPublic;
	}
	
	public String getThumbnail() {
		return thumbnail;
	}
	public void setThumbnail(String thumbnail) {
		this.thumbnail = thumbnail;
	}
	
	public List<PlayNode> getNodes() {
		return nodes;
	}
	public void setNodes(List<PlayNode> nodes) {
		this.nodes = nodes;
	}
}
