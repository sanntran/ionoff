package net.xapxinh.center.shared.dto;

import java.util.Collections;
import java.util.List;

public class PlayListDto extends SerializableDto {

	private static final long serialVersionUID = 1L;
	
	private long id;
	private String name;
	private String thumbnail;
	private Boolean isPublic;
	private Integer listenCount;
	private List<PlayNodeDto> nodes;

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

	public String getThumbnail() {
		return thumbnail;
	}

	public void setThumbnail(String thumbnail) {
		this.thumbnail = thumbnail;
	}

	public Boolean getIsPublic() {
		return isPublic;
	}

	public void setIsPublic(Boolean isPublic) {
		this.isPublic = isPublic;
	}

	public Integer getListenCount() {
		return listenCount;
	}

	public void setListenCount(Integer listenCount) {
		this.listenCount = listenCount;
	}
	
	public List<PlayNodeDto> getNodes() {
		if (nodes == null) {
			return Collections.emptyList();
		}
		return this.nodes;
	}

	public void setNodes(List<PlayNodeDto> nodes) {
		this.nodes = nodes;
	}
	
	public boolean isNew() {
		return id == 0L;
	}
}
