package net.xapxinh.dataservice.parser;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import java.util.List;

@JacksonXmlRootElement(localName = "album")
public class Album {

    @JacksonXmlProperty(localName = "title")
    private String title;

    @JacksonXmlProperty(localName = "releaseDate")
    private String releaseDate;

    @JacksonXmlProperty(localName = "description")
    private String description;

    @JacksonXmlProperty(localName = "artists")
    private String artists;

    @JacksonXmlProperty(localName = "authors")
    private String authors;

    @JacksonXmlElementWrapper(localName = "musicFiles")
    @JacksonXmlProperty(localName = "musicFile")
    private List<MusicFile> musicFiles;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getReleaseDate() {
        return releaseDate;
    }

    public void setReleaseDate(String releaseDate) {
        this.releaseDate = releaseDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getArtists() {
        return artists;
    }

    public void setArtists(String artists) {
        this.artists = artists;
    }

    public String getAuthors() {
        return authors;
    }

    public void setAuthors(String authors) {
        this.authors = authors;
    }

    public List<MusicFile> getMusicFiles() {
        return musicFiles;
    }

    public void setMusicFiles(List<MusicFile> musicFiles) {
        this.musicFiles = musicFiles;
    }

    public boolean hasMusicFile(MusicFile musicFile) {
        for (MusicFile file : musicFiles) {
            if (file.getTitle().equals(musicFile.getTitle())) {
                return true;
            }
        }
        return false;
    }
}
