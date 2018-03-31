/*
 * Copyright (c) 2012 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package net.xapxinh.dataservice.youtube.api;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;

import com.google.api.client.http.HttpRequest;
import com.google.api.client.http.HttpRequestInitializer;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.client.util.DateTime;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.ResourceId;
import com.google.api.services.youtube.model.SearchListResponse;
import com.google.api.services.youtube.model.SearchResult;
import com.google.api.services.youtube.model.Thumbnail;
import com.google.api.services.youtube.model.Video;

/**
 * Print a list of videos matching a search term.
 *
 * @author Jeremy Walker
 */
public class YoutubeApi3 {

	private static Logger logger = Logger.getLogger(YoutubeApi3.class.getName());

	/**
	 * Define a global variable that identifies the name of a file that
	 * contains the developer's API key.
	 */
	private static final String PROPERTIES_FILENAME = "youtube.properties";

	private static final String APP_NAME = "XapXinh";

	private static final long NUMBER_OF_VIDEOS_RETURNED = 10;

	private final HttpTransport HTTP_TRANSPORT;
	/**
	 * Define a global instance of the JSON factory.
	 */
	private final JsonFactory JSON_FACTORY;
	private final String YOUTUBE_APIKEY;

	/**
	 * Define a global instance of a Youtube object, which will be used
	 * to make YouTube Data API requests.
	 */
	private static YouTube youtube;

	private final Properties properties;

	public YoutubeApi3() {
		HTTP_TRANSPORT = new NetHttpTransport();
		JSON_FACTORY = new JacksonFactory();

		properties = new Properties();
		try {
			final InputStream in = YoutubeApi3.class.getResourceAsStream("/" + PROPERTIES_FILENAME);
			properties.load(in);
		} catch (final IOException e) {
			logger.error("There was an error reading "
					+ PROPERTIES_FILENAME + ": "
					+ e.getCause() + " : "
					+ e.getMessage());
		}
		YOUTUBE_APIKEY = properties.getProperty("youtube.apikey");
	}

	public YoutubeVideos searchVideos(String query, String pageToken) throws IOException {

		// This object is used to make YouTube Data API requests. The last
		// argument is required, but since we don't need anything
		// initialized when the HttpRequest is initialized, we override
		// the interface and provide a no-op function.
		youtube = new YouTube.Builder(HTTP_TRANSPORT, JSON_FACTORY, new HttpRequestInitializer() {
			@Override
			public void initialize(HttpRequest request) throws IOException {
			}
		}).setApplicationName(APP_NAME).build();

		final String queryTerm = query;

		final YouTube.Search.List search = youtube.search().list("id,snippet");

		if (pageToken != null && !"null".equals(pageToken)) {
			search.setPageToken(pageToken);
		}

		search.setKey(YOUTUBE_APIKEY);
		search.setQ(queryTerm);

		// Restrict the search results to only include videos. See:
		// https://developers.google.com/youtube/v3/docs/search/list#type
		search.setType("video");

		// To increase efficiency, only retrieve the fields that the
		// application uses.
		search.setFields("items(id/kind"
				+ ",id/videoId"
				+ ",snippet/title"
				+ ",snippet/thumbnails/medium/url"
				+ ",snippet/publishedAt)"
				+ ",prevPageToken"
				+ ",nextPageToken");
		search.setMaxResults(NUMBER_OF_VIDEOS_RETURNED);
		
		// Call the API and print results.
		final SearchListResponse searchResponse = search.execute();

		final YoutubeVideos youtubeVideos = new YoutubeVideos();
		youtubeVideos.setPageToken(pageToken);

		youtubeVideos.setPrevPageToken(searchResponse.getPrevPageToken());
		youtubeVideos.setNextPageToken(searchResponse.getNextPageToken());

		final List<SearchResult> searchResultList = searchResponse.getItems();
		youtubeVideos.setVideos(prettyPrint(searchResultList.iterator()));

		return youtubeVideos;
	}

	/*
	 * Prints out all results in the Iterator. For each result, print the
	 * title, video ID, and thumbnail.
	 *
	 * @param iteratorSearchResults Iterator of SearchResults to print
	 *
	 * @param query Search query (String)
	 */
	private List<YoutubeVideo> prettyPrint(Iterator<SearchResult> iteratorSearchResults) throws IOException {

		final List<YoutubeVideo> youtubeVideos = new ArrayList<YoutubeVideo>();

		if (iteratorSearchResults == null || !iteratorSearchResults.hasNext()) {
			return youtubeVideos;
		}

		String ids = "";

		while (iteratorSearchResults.hasNext()) {
			final YoutubeVideo youtubeVideo = new YoutubeVideo();
			final SearchResult singleVideo = iteratorSearchResults.next();
			final ResourceId resourceId = singleVideo.getId();

			// Confirm that the result represents a video. Otherwise, the
			// item will not contain a video ID.
			if (resourceId.getKind().equals("youtube#video")) {
				final Thumbnail thumbnail = singleVideo.getSnippet().getThumbnails().getMedium();
				youtubeVideo.setId(resourceId.getVideoId());
				youtubeVideo.setThumbnail(thumbnail.getUrl());
				youtubeVideo.setTitle(singleVideo.getSnippet().getTitle());
				youtubeVideo.setUploadedTime(fortmatDate(singleVideo.getSnippet().getPublishedAt()));
				youtubeVideos.add(youtubeVideo);

				ids = ids + "," + youtubeVideo.getId();
			}
		}

		if (youtubeVideos.isEmpty()) {
			return youtubeVideos;
		}

		final YouTube.Videos.List list = youtube.videos().list("statistics, contentDetails");
		list.setId(ids.substring(1)); // remove first comma
		list.setKey(YOUTUBE_APIKEY);

		final List<Video>  videos = list.execute().getItems();
		return putStatistic(youtubeVideos, videos);
	}

	private List<YoutubeVideo> putStatistic(List<YoutubeVideo> youtubeVideos, List<Video> videos) {
		for (final Video video : videos) {
			final YoutubeVideo youtubeVideo = getYoutubeVideo(youtubeVideos, video.getId());
			if (youtubeVideo != null) {
				if (video.getContentDetails() != null && video.getContentDetails().getDuration() !=null) {
					youtubeVideo.setDuration(toSeconds(video.getContentDetails().getDuration()));
				}
				else {
					youtubeVideo.setDuration(0);
				}
				if (video.getStatistics() != null) {
					if (video.getStatistics().getViewCount() != null) {
						youtubeVideo.setViewCount(video.getStatistics().getViewCount().longValue());
					}
					else {
						youtubeVideo.setViewCount(0);
					}
					if (video.getStatistics().getFavoriteCount() != null) {
						youtubeVideo.setFavoriteCount(video.getStatistics().getFavoriteCount().longValue());
					}
					else {
						youtubeVideo.setFavoriteCount(0);
					}
				}
				else {
					youtubeVideo.setViewCount(0);
					youtubeVideo.setFavoriteCount(0);
				}
			}
		}
		return youtubeVideos;
	}

	private long toSeconds(String youtubeDuration) {

		String time = youtubeDuration.substring(2);
		long duration = 0L;
		final Object[][] indexs = new Object[][]{{"H", 3600}, {"M", 60}, {"S", 1}};
		for (final Object[] index2 : indexs) {
			final int index = time.indexOf((String) index2[0]);
			if(index != -1) {
				final String value = time.substring(0, index);
				duration += Integer.parseInt(value) * (int) index2[1];
				time = time.substring(value.length() + 1);
			}
		}
		return duration;
	}

	private YoutubeVideo getYoutubeVideo(List<YoutubeVideo> youtubeVideos, String id) {
		for (final YoutubeVideo youtubeVideo : youtubeVideos) {
			if (id.equals(youtubeVideo.getId())) {
				return youtubeVideo;
			}
		}
		return null;
	}

	private static final String DATE_FORTMAT = "dd/MM/yyyy";

	private String fortmatDate(DateTime dateTime) {
		if (dateTime == null) {
			return "";
		}
		final Date date = new Date(dateTime.getValue());
		final SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORTMAT);
		return sdf.format(date);
	}
}