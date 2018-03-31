package net.xapxinh.dataservice.webmvc;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.xapxinh.dataservice.exception.UnknownPlayerException;
import net.xapxinh.dataservice.exception.UrlNotAvailableException;
import net.xapxinh.dataservice.exception.YoutubeDLException;
import net.xapxinh.dataservice.persistence.service.PlayerService;
import net.xapxinh.dataservice.youtube.YoutubeService;
import net.xapxinh.dataservice.youtube.api.YoutubeVideos;

@RestController
@EnableWebMvc
public class YoutubeServiceController {

	private static final  Logger LOGGER = Logger.getLogger(YoutubeServiceController.class.getName());

	@Autowired
	private PlayerService playerService;

	@Autowired
	private YoutubeService youtubeService;

	@RequestMapping(value = "videos/search",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public YoutubeVideos searchAlbums(@RequestParam("mac") String mac,
			@RequestParam("key") String key,
			@RequestParam("pageToken") String pageToken) throws UnknownPlayerException, JSONException, IOException {

		playerService.findByMac(mac);
		final YoutubeVideos videos = youtubeService.searchYoutubeVideos(key, pageToken);
		return videos;
	}
	
	@RequestMapping(value = "videos/{id}/videourl",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public String getVideoUrl(@RequestParam("mac") String mac,
			@PathVariable("id") String id) throws UnknownPlayerException, YoutubeDLException, UrlNotAvailableException {

		playerService.findByMac(mac);
		return youtubeService.getVideoUrl(id);
	}
	
	@RequestMapping(value = "videos/{id}/audiourl",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public String getAudioUrl(@RequestParam("mac") String mac,
			@PathVariable("id") String id) throws UnknownPlayerException, YoutubeDLException, UrlNotAvailableException {

		playerService.findByMac(mac);
		return youtubeService.getAudioUrl(id);
	}

	@ExceptionHandler(UnknownPlayerException.class)
	@ResponseBody
	public DataServiceMesage handleException(HttpServletRequest request, HttpServletResponse response, UnknownPlayerException e) {
		LOGGER.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
		return new DataServiceMesage(HttpServletResponse.SC_BAD_REQUEST + "",
				e.getMessage(), UnknownPlayerException.class.getSimpleName());
	}
}
