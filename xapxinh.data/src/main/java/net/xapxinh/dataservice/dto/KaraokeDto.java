package net.xapxinh.dataservice.dto;

public class KaraokeDto {
	
	private long id;
	private String title;	
	private String author;
	private String singer;
	private String type;
	private String lyric;
	private Long favouriteCount;	

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public String getSinger() {
		return singer;
	}

	public void setSinger(String singer) {
		this.singer = singer;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getLyric() {
		return lyric;
	}

	public void setLyric(String lyric) {
		this.lyric = lyric;
	}

	public Long getFavouriteCount() {
		return favouriteCount;
	}

	public void setFavouriteCount(Long favouriteCount) {
		this.favouriteCount = favouriteCount;
	}
}
