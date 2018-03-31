package net.xapxinh.dataservice.webmvc;

import javax.servlet.http.HttpServletResponse;

import net.xapxinh.dataservice.entity.EFactory;
import net.xapxinh.dataservice.entity.Player;
import net.xapxinh.dataservice.exception.UnknownPlayerException;
import net.xapxinh.dataservice.persistence.service.PlayerService;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@RestController
@EnableWebMvc
public class PlayerServiceController {

	private static final Logger LOGGER = Logger.getLogger(PlayerServiceController.class.getName());

	@Autowired
	private PlayerService playerService;

	@RequestMapping(value = "players",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public DataServiceMesage insertPlayer(@RequestBody String mac) throws UnknownPlayerException {

		if (mac == null || mac.isEmpty()) {
			throw new UnknownPlayerException(mac);
		}
		try {
			playerService.findByMac(mac);
		}
		catch (final UnknownPlayerException e) {
			final Player player = EFactory.newPlayer();
			player.setMac(mac);
			playerService.insert(player);
			LOGGER.info("New player has been inserted: Mac: " + mac + " Name: " + player.getMac());
		}
		return new DataServiceMesage(HttpServletResponse.SC_OK + "", mac);
	}

	@ExceptionHandler(UnknownPlayerException.class)
	@ResponseBody
	public DataServiceMesage handleException(HttpServletResponse response, UnknownPlayerException e) {
		LOGGER.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
		return new DataServiceMesage(HttpServletResponse.SC_BAD_REQUEST + "", e.getMessage(), UnknownPlayerException.class.getSimpleName());
	}
}
