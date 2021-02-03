package net.ionoff.csnlink.downloader;

import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.mpatric.mp3agic.*;
import net.ionoff.csnlink.model.Album;
import net.ionoff.csnlink.model.Link;
import net.ionoff.csnlink.model.MusicFile;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.stream.Collectors;

@Component
public class CSNDownloader extends Thread {

    private static String LINK_URL = "http://cloud.ionoff.net/csnlink/links";
    private static final Logger logger = LoggerFactory.getLogger(CSNDownloader.class);

    public static void main(String[] args) {
        new CSNDownloader().start();
    }

    private RestTemplate restTemplate;

    private CSNDownloader() {
        this.restTemplate = new RestTemplate();
    }

    private void updateLink(Link link) {
        try {
            restTemplate.postForObject(LINK_URL + "/" + link.getId(), link, Link.class);
            logger.info("------------- Updated link status {}", link.toString());
        } catch (Exception e) {
            logger.error("!!!!!!!!!!!! Error when update link status {}", e.getMessage(), e);
        }
    }

    public static class LinkList extends ArrayList<Link> {
        public LinkList() {
            super();
        }
    }

    private Optional<Link> getPendingLink() {
        try {
            LinkList links = restTemplate.getForObject(LINK_URL + "?status=PENDING", LinkList.class);
            logger.info("------------- Get pending link, found {} link to download", links.size());
            return links.isEmpty() ? Optional.empty() : Optional.of(links.get(0));
        } catch (Exception e) {
            logger.error("!!!!!!!!!!!! Error when get pending link {}", e.getMessage(), e);
            return Optional.empty();
        }
    }

    @Override
    public void run() {
        for (; true; ) {
            Optional<Link> linkOptional = getPendingLink();
            if (!linkOptional.isPresent()) {
                logger.info("------------- no link to download, wait 60 seconds");
                try {
                    Thread.sleep(60000);
                } catch (InterruptedException e) {
                    logger.error("!!!!!!!!!!!! InterruptedException {}", e.getMessage(), e);
                }
                continue;
            }
            Link link = linkOptional.get();
            Link.Status status = tryCreateAlbum(link);
            link.setStatus(status);
            updateLink(link);
        }
    }

    public static Link.Status tryCreateAlbum(Link link) {
        try {
            createAlbum(link);
            return Link.Status.PROCESSED;
        } catch (Exception e) {
            logger.error("Error when download album {} {}. {}", link.toString(), e);
            return Link.Status.ERROR;
        }
    }

    public static void createAlbum(Link link) throws Exception {
        String albumLink = link.getLink();
        Album album = new Album();
        album.setMusicFiles(new ArrayList<>());

        String albumFolder = null;
        for (int i = 1; i < 30; i++) {
            MusicFile musicFile = parseMusicFile(albumLink + "?playlist=" + i);
            if (album.hasMusicFile(musicFile)) {
                break;
            } else {
                musicFile.setName(String.format("%02d", i)  + ".mp3");
                album.getMusicFiles().add(musicFile);
                String file = FileDownloader.downloadFile(
                        (String.format("%02d", i) + "-" + musicFile.getTitle()),
                        musicFile.getAlbum(), musicFile.getDownloadLink());
                if (i == 1) {
                    albumFolder = (new File(file).getParentFile().getAbsolutePath());
                    if (musicFile.getAlbumImageLink() != null) {
                        FileDownloader.downloadFile("album",
                                musicFile.getAlbum(),
                                musicFile.getAlbumImageLink());
                    }
                }

                updateTags(file,
                        String.format("%02d", i),
                        musicFile.getTitle(),
                        musicFile.getArtists(),
                        musicFile.getAuthor(),
                        musicFile.getAlbum(),
                        album.getArtists());

            }
            if (musicFile.getReleaseDate() != null && album.getReleaseDate() == null) {
                album.setReleaseDate(musicFile.getReleaseDate());
            }
            if (musicFile.getAlbum() != null && album.getTitle() == null) {
                album.setTitle(musicFile.getAlbum());
            }
            if (musicFile.getArtists() != null) {
                if (album.getArtists() == null) {
                    album.setArtists(musicFile.getArtists());
                } else {
                    album.setArtists(album.getArtists() + ", " + musicFile.getArtists());
                }
                album.setTitle(musicFile.getAlbum());
            }
        }

        if (album.getArtists() != null) {
            List<String> artists = Arrays.asList(album.getArtists().split(","));
            Set<String> artistSet = artists.stream().map(String::trim).collect(Collectors.toCollection(LinkedHashSet::new));
            album.setArtists(String.join(",", artistSet));
        }

        writeXml(albumFolder, album);
        writeDb(albumFolder);
    }

    private static void writeDb(String albumFolder) {
        RestTemplate restTemplate = new RestTemplate();
        String folder = albumFolder.replaceAll("\\\\", "/");
        String html = restTemplate.getForObject(
                "http://localhost:8081/imedia/api/albums/scan?folder=" + folder,
                String.class);
        logger.info("!!!!!!!!!!!!!!!! write album to DB: " + html);
        if (html.contains("Exception")) {
            throw new RuntimeException("Error write album to DB " + html);
        }
    }

    public static MusicFile parseMusicFile(String csnLink) throws IOException {
        RestTemplate restTemplate = new RestTemplate();
        String html = restTemplate.getForObject(csnLink, String.class);
        Document doc = Jsoup.parse(html);

        MusicFile musicFile = new MusicFile();
        Element artistElement =
                doc.select("li:contains(Ca sĩ:)")
                .get(0).getAllElements()
                .get(2);
        String title = artistElement.parent().parent().parent()
                    .getAllElements().get(1).text();
        String artists = artistElement.text()
                .replaceAll("; ", ", ")
                .replaceAll(";", ", ");

        Elements authorElements = doc.select("li:contains(Sáng tác:)");
        if (authorElements != null && !authorElements.isEmpty()) {
            String author = authorElements.get(0).getAllElements()
                    .get(2).text();
            musicFile.setAuthor(author);
        } else {
            musicFile.setAuthor("Unknown");
        }
        String album = doc.select("li:contains(Album:)")
                .get(0).getAllElements()
                .get(2).text();

        Elements imageElements = doc.select("div#companion_cover");
        if (imageElements != null && !imageElements.isEmpty()) {
            String img = imageElements.get(0).getElementsByAttribute("src").get(0).attr("src");
            musicFile.setAlbumImageLink(img);
        }

        Elements releaseDateElements = doc.select("li:contains(Năm phát hành:)");
        if (releaseDateElements != null && !releaseDateElements.isEmpty()) {
            String releaseDate = releaseDateElements.get(0).getAllElements().get(0).text().substring("Năm phát hành: ".length());
            musicFile.setReleaseDate(releaseDate);
        }
        musicFile.setTitle(title);
        musicFile.setArtists(artists);

        musicFile.setDownloadLink(getDownloadLink(html));
        musicFile.setAlbum(album);
        return musicFile;
    }

    private static String getDownloadLink(String html) {
        String[] lines = html.split("\n");
        for (String line : lines) {
            if (!line.isEmpty() && line.charAt(line.length() - 1) == ','
                    && line.contains(".mp3")
                    && line.contains("\"file\": \"")) {
                String link = line.substring(
                        line.lastIndexOf("\"file\": \"") + "\"file\": \"".length(),
                        line.lastIndexOf(".mp3"));
                return link.replace("/128/", "/320/") + ".mp3";
            }
        }
        throw new RuntimeException("Link not found");
    }

    public static void writeXml(String folder, Album album) throws IOException {
        XmlMapper xmlMapper = new XmlMapper();
        xmlMapper.enable(SerializationFeature.INDENT_OUTPUT);
        xmlMapper.writeValue(new File(folder + File.separator + "_info.xml"),
                album);
    }
    private static void updateTags(String file,
                                   String track,
                                   String title,
                                   String artists,
                                   String composer,
                                   String album,
                                   String albumArtists) throws InvalidDataException, IOException, UnsupportedTagException, NotSupportedException {
        logger.info("Updating tags for file={} track={} title={} artists={} composer={} album={} albumArtists={}",
                file, track, title, artists, composer, album, albumArtists);
        Mp3File mp3file = new Mp3File(file);

        if (mp3file.hasId3v1Tag()) {
            mp3file.removeId3v1Tag();
        }
        if (mp3file.hasCustomTag()) {
            mp3file.removeCustomTag();
        }
        if (mp3file.hasId3v2Tag()) {
            mp3file.removeId3v2Tag();
        }

        ID3v2 id3v2Tag = new ID3v24Tag();
        id3v2Tag.setTrack(track);
        id3v2Tag.setTitle(title);
        id3v2Tag.setArtist(artists);
        id3v2Tag.setComposer(composer);
        id3v2Tag.setAlbum(album);
        id3v2Tag.setAlbumArtist(albumArtists);

        mp3file.setId3v2Tag(id3v2Tag);

        String newFile = file + ".tmp";
        mp3file.save(newFile);

        Files.move(new File(newFile).toPath(), new File(file).toPath(),
                StandardCopyOption.REPLACE_EXISTING);

        logger.info("Updated tags for file {} ", file);
    }

}
