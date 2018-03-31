package net.xapxinh.dataservice.youtube;

import java.io.IOException;

import net.xapxinh.dataservice.exception.UrlNotAvailableException;
import net.xapxinh.dataservice.exception.YoutubeDLException;
import net.xapxinh.dataservice.youtube.api.YoutubeVideos;

public interface YoutubeService {

	public YoutubeVideos searchYoutubeVideos(final String key, final String pageToken) throws IOException;
	
	public String getVideoUrl(final String videoId) throws YoutubeDLException, UrlNotAvailableException;
	
	public String getAudioUrl(final String videoId) throws YoutubeDLException, UrlNotAvailableException;
}
