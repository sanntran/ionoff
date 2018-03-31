package net.xapxinh.dataservice.webmvc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

import net.xapxinh.dataservice.dto.AlbumDto;
import net.xapxinh.dataservice.dto.mapper.AlbumDtoMapping;
import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.exception.UnknownPlayerException;
import net.xapxinh.dataservice.persistence.service.AlbumService;
import net.xapxinh.dataservice.persistence.service.PlayerService;
import net.xapxinh.dataservice.search.service.IAlbumSearchService;
import net.xapxinh.dataservice.servlet.util.ServletUtils;

@RestController
@EnableWebMvc
public class AlbumServiceController {

	private static final  Logger LOGGER = Logger.getLogger(AlbumServiceController.class.getName());

	@Autowired
	private PlayerService playerService;

	@Autowired
	private AlbumService albumService;

	@Autowired
	private IAlbumSearchService albumSearchService;


	@RequestMapping(value = "albums/scan",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public String scanAlbums(HttpServletRequest httpRequest) {
		try {
			return AlbumCreator.scanAlbumFolder(httpRequest);
		}
		catch (final Exception e) {
			LOGGER.error(e.getMessage(), e);
			return e.getClass() + ": " + e.getMessage();
		}
	}

	@RequestMapping(value = "albums/search",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<AlbumDto> searchAlbums(@RequestParam("mac") String mac,
			@RequestParam("searchKey") String searchKey,
			@RequestParam("searchScope") String searchScope,
			@RequestParam("pageNumber") Integer pageNumber,
			@RequestParam("pageSize") Integer pageSize) throws UnknownPlayerException, JSONException, IOException {

		playerService.findByMac(mac);
		final List<AlbumDto> albums = searchAlbums(searchKey, searchScope, pageNumber, pageSize);
		if (!albums.isEmpty()) {
			Map<Long, Integer> albumListenCountMap = getAlbumListenCounts(albums);
			for (final AlbumDto album : albums) {
				album.setListenCount(albumListenCountMap.get(album.getId()));
				album.setImage(AlbumDtoMapping.getAlbumImageUrl(album.getName()));
			}
		}
		return albums;
	}

	/**
	 * This method is to get album listen count from DB
	 */
	private Map<Long, Integer> getAlbumListenCounts(List<AlbumDto> albums) {
		List<Long> albumIds = new ArrayList<>(albums.size());
		for (AlbumDto album : albums) {
			albumIds.add(album.getId());
		}
		return albumService.getListenCounts(albumIds);
	}

	private List<AlbumDto> searchAlbums(String searchKey, String searchScope, int pageNumber, int pageSize) throws JSONException, IOException {
		if ("title".equals(searchScope)) {
			return albumSearchService.searchByTitle(searchKey, pageNumber);
		}
		else if ("singer".equals(searchScope)) {
			return albumSearchService.searchByArtistName(searchKey, pageNumber);
		}
		else if ("author".equals(searchScope)) {
			return albumSearchService.searchByAuthorName(searchKey, pageNumber);
		}
		return albumSearchService.searchAll(searchKey, pageNumber);
	}

	@RequestMapping(value = "albums/{albumId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public AlbumDto getAlbum(@PathVariable("albumId") Long albumId,
			@RequestParam("mac") String mac, HttpServletRequest httpRequest) throws UnknownPlayerException {
		playerService.findByMac(mac);

		final Album album = albumService.findById(albumId);
		final String baseUrl = ServletUtils.getBaseUrl(httpRequest);
		return AlbumDtoMapping.toAlbumDtoIncludingSongs(album, baseUrl, mac);
	}

	@RequestMapping(value = "albums/{albumId}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public DataServiceMesage increaseAlbumListenCount(@PathVariable("albumId") Long albumId,
			@RequestParam("mac") String mac, HttpServletRequest httpRequest) throws UnknownPlayerException {
		playerService.findByMac(mac);
		final Album album = albumService.findById(albumId);
		if (album.getListenCount() == null) {
			album.setListenCount(1);
		}
		else {
			album.setListenCount(album.getListenCount().intValue() + 1);
		}
		albumService.update(album);
		return new DataServiceMesage(HttpServletResponse.SC_OK + "", album.getListenCount() + "");
	}

	@RequestMapping(value = "albums/special",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<AlbumDto> getSpecialAlbums(@RequestParam("mac") String mac
			, HttpServletRequest httpRequest) throws Exception {

		playerService.findByMac(mac);
		final String baseUrl = ServletUtils.getBaseUrl(httpRequest);
		final List<Album> specialAlbums = new ArrayList<Album>();
		final List<Album> mostListenCountAlbums = albumService.getMostListenCount();
		final List<Album> recentlyUploadedAlbums = albumService.getLatestUploaded();

		specialAlbums.addAll(mostListenCountAlbums);
		specialAlbums.addAll(recentlyUploadedAlbums);

		final List<AlbumDto> albumDtos = new ArrayList<AlbumDto>();
		for (final Album album : specialAlbums) {
			final AlbumDto albumDto = AlbumDtoMapping.toAlbumDto(album, baseUrl, mac);
			albumDtos.add(albumDto);
		}
		return albumDtos;
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
