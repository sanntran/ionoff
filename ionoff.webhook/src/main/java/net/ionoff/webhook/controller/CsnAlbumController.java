package net.ionoff.webhook.controller;

import com.fasterxml.jackson.databind.JsonNode;
import net.ionoff.webhook.dto.CsnAlbumDto;
import net.ionoff.webhook.dto.CsnAlbumListDto;
import net.ionoff.webhook.dto.MessageDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping(value = "/csn-albums")
public class CsnAlbumController {
	
	private static final Logger logger = LoggerFactory.getLogger(CsnAlbumController.class);

	@Autowired
	private CSNDownloader csnDownloader;

	@RequestMapping(value = "",
			method = RequestMethod.POST,
			consumes = "application/json; charset=utf-8",
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto update(@RequestBody List<CsnAlbumDto> albums,
							 HttpServletRequest request) throws IOException {
		csnDownloader.downloadAlbums(albums);
		MessageDto messageDto = new MessageDto();
		messageDto.setMessage("Added to queue");
		return messageDto;
	}

}
