package net.xapxinh.center.server.entity;

import java.io.Serializable;

public class PlayLeaf implements Serializable {

	private static final long serialVersionUID = 1L;

	public enum TYPE {
		file, youtube;
	}
	private long id;
	private String name;
	private Integer idx;
	private String type;
	private String image;
	private String duration;
	private String url;
	private String mrl;
	private String authors;
	private String artists;
	
	private PlayNode playNode;
	
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

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getMrl() {
		return mrl;
	}

	public void setMrl(String mrl) {
		this.mrl = mrl;
	}

	public String getImage() {
		return image;
	}

	public void setImage(String img) {
		this.image = img;
	}

	public String getAuthors() {
		return authors;
	}

	public void setAuthors(String authors) {
		this.authors = authors;
	}

	public String getArtists() {
		return artists;
	}

	public void setArtists(String artists) {
		this.artists = artists;
	}

	public PlayNode getPlayNode() {
		return playNode;
	}

	public void setPlayNode(PlayNode playNode) {
		this.playNode = playNode;
	}
}