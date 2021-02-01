package net.xapxinh.dataservice.webmvc;

import net.xapxinh.dataservice.dto.AlbumDto;
import net.xapxinh.dataservice.dto.SongDto;
import net.xapxinh.dataservice.dto.mapper.AlbumDtoMapping;
import net.xapxinh.dataservice.entity.Song;
import net.xapxinh.dataservice.exception.UnknownPlayerException;
import net.xapxinh.dataservice.persistence.service.PlayerService;
import net.xapxinh.dataservice.persistence.service.SongService;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@RestController
@EnableWebMvc
public class SongServiceController {

	private static final  Logger LOGGER = Logger.getLogger(AlbumServiceController.class.getName());

	@Autowired
	private PlayerService playerService;

	@Autowired
	private SongService songService;


	@RequestMapping(value = "songs/{songId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SongDto getSong(@PathVariable("songId") Long songId,
							@RequestParam("mac") String mac, HttpServletRequest httpRequest) throws UnknownPlayerException {
		//playerService.findByMac(mac);
		final Song song = songService.findById(songId);
		return AlbumDtoMapping.toSongDto(song);
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
