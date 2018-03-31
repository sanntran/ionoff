package net.xapxinh.dataservice.youtube;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import net.xapxinh.dataservice.config.DataServiceConfig;
import net.xapxinh.dataservice.exception.UrlNotAvailableException;
import net.xapxinh.dataservice.exception.YoutubeDLException;
import net.xapxinh.dataservice.exception.YoutubeDLInfoException;
import net.xapxinh.dataservice.youtube.api.YoutubeApi3;
import net.xapxinh.dataservice.youtube.api.YoutubeVideos;

public class YoutubeServiceImpl implements YoutubeService {
	
	private static final Logger LOGGER = Logger.getLogger(YoutubeServiceImpl.class.getName());
	
	private final YoutubeApi3 youtubeApi;

	public YoutubeServiceImpl(final YoutubeApi3 youtubeApi) {
		this.youtubeApi = youtubeApi;
	}

	@Override
	public YoutubeVideos searchYoutubeVideos(final String key, final String pageToken) throws IOException {
		return youtubeApi.searchVideos(key, pageToken);
	}

	@Override
	public String getVideoUrl(String videoId) throws YoutubeDLException, UrlNotAvailableException {
		
		String videoFormatId = getVideoFormatId(videoId);
		
		if (videoFormatId == null || videoFormatId.isEmpty()) {
			throw new UrlNotAvailableException("Unable to get video format for id " + videoId);
		}
		
		String url = getUrlOfVideoFormat(videoId, videoFormatId);
		if (url == null) {
			throw new UrlNotAvailableException("Unable to get video url (format " + videoFormatId + ") for id " + videoId);
		}
		return url;
	}

	private String getVideoFormatId(String videoId) throws YoutubeDLInfoException {
		List<String> videoInfoLines = getVideoInfo(videoId);
		for (int i = videoInfoLines.size() - 1; i >= 0; i--) {
			String line = videoInfoLines.get(i);
			if (!line.contains("video only") && !line.contains("audio only")) {
				String id = "";
				for (int j = 0; j < line.length(); j++) {
					if (Character.isDigit(line.charAt(j))) {
						id = id + line.charAt(j);
					}
					else {
						return id;
					}
				}
			}
		}
		return null;
	}

	@Override
	public String getAudioUrl(String videoId) throws YoutubeDLException, UrlNotAvailableException {
		String url = null;
		for (String format : DataServiceConfig.getInstance().YOUTUBE_AUDIO_FORMATS) {
			url = getUrlOfVideoFormat(videoId, format);
			if (url == null) {
				LOGGER.info("ERROR: requested format " + format + " not available for id " + videoId);
			}
			else {
				return url;
			}
		}
		throw new UrlNotAvailableException("Unable to get audio url for id " + videoId);
	}
	
	private String getUrlOfVideoFormat(String videoId, String format) throws YoutubeDLException {
		
		try {
			Process pf = Runtime.getRuntime().exec("youtube-dl --prefer-insecure -g -f" + format + " " + videoId);

			pf.waitFor();
			BufferedReader buf = new BufferedReader(new InputStreamReader(pf.getInputStream()));
			List<String> urls = new ArrayList<>();
			
			String line = null;

			while ((line = buf.readLine()) != null) {
				urls.add(line);
			}

			if (!urls.isEmpty() && urls.get(0).startsWith("http")) {
				return urls.get(0);
			}
			
			return null;
		}
		catch (Exception e) {
			throw new YoutubeDLException(e.getMessage());
		}
		
	}

	private List<String> getVideoInfo(String videoId) throws YoutubeDLInfoException {

		try {
			List<String> infoLines = new ArrayList<>();
			Process p = Runtime.getRuntime().exec("youtube-dl -F " + videoId);
			p.waitFor();
			BufferedReader buf = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line = "";
			while ((line = buf.readLine()) != null) {
				infoLines.add(line);
			}
			if (!isValidVideoInfo(infoLines)) {
				throw new YoutubeDLInfoException("Error orrcured when getting video info");
			}
			return infoLines;
			
		} catch (Exception e) {
			throw new YoutubeDLInfoException(e.getMessage());
		}
	}

	private boolean isValidVideoInfo(List<String> infoLines) {
		if (infoLines.isEmpty()) {
			return false;
		}
		// check last line starts with number
		if (!isStartWithNumber(infoLines.get(infoLines.size() - 1))) {
			return false;
		}

		for (int i = infoLines.size() - 2; i >= 0; i--) {
			String line = infoLines.get(i);
			if (!isStartWithNumber(line)) {
				if (line.startsWith("format")) {
					return true;
				} else {
					return false;
				}
			}
		}
		return false;
	}

	private boolean isStartWithNumber(String string) {
		if (Character.isDigit(string.charAt(0))) {
			return true;
		}
		return false;
	}
}
