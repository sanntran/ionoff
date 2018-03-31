package net.xapxinh.center.shared.dto;

import java.util.List;

public class YoutubeVideos extends SerializableDto {
	
	private static final long serialVersionUID = 1L;
	
	private String pageToken;
	private String prevPageToken;
	private String nextPageToken;
	
	private List<YoutubeVideo> videos;

	public String getPageToken() {
		return pageToken;
	}

	public void setPageToken(String pageToken) {
		this.pageToken = pageToken;
	}

	public String getPrevPageToken() {
		return prevPageToken;
	}

	public void setPrevPageToken(String prevPageToken) {
		this.prevPageToken = prevPageToken;
	}

	public String getNextPageToken() {
		return nextPageToken;
	}

	public void setNextPageToken(String nextPageToken) {
		this.nextPageToken = nextPageToken;
	}

	public List<YoutubeVideo> getVideos() {
		return videos;
	}

	public void setVideos(List<YoutubeVideo> videos) {
		this.videos = videos;
	}
}