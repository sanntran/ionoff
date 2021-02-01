package net.ionoff.webhook.dto;

public class CsnAlbumDto {

    private String link;
    private String title;
    private String artists;
    private String releaseDate;

    public CsnAlbumDto() {}

    public CsnAlbumDto(String link, String title, String artists) {
        this.link = link;
        this.title = title;
        this.artists = artists;
    }
    public CsnAlbumDto(String link, String title, String artists, String releaseDate) {
        this.link = link;
        this.title = title;
        this.artists = artists;
        this.releaseDate = releaseDate;
    }

    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getArtists() {
        return artists;
    }

    public void setArtists(String artists) {
        this.artists = artists;
    }

    public String getReleaseDate() {
        return releaseDate;
    }

    public void setReleaseDate(String releaseDate) {
        this.releaseDate = releaseDate;
    }

    @Override
    public String toString() {
        return "CsnAlbumDto{" +
                "link='" + link + '\'' +
                ", title='" + title + '\'' +
                ", artists='" + artists + '\'' +
                ", releaseDate='" + releaseDate + '\'' +
                '}';
    }
}
