package net.xapxinh.dataservice.parser;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;

public class MusicFile {

    @JacksonXmlProperty(isAttribute = true)
    private String name;

    @JacksonXmlProperty(isAttribute = true)
    private String title;

    @JacksonXmlProperty(isAttribute = true)
    private String artists;

    @JacksonXmlProperty(isAttribute = true)
    private String author;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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

    public String getAuthor() {
        return author;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

}
