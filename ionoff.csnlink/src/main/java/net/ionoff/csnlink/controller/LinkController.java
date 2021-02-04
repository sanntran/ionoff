package net.ionoff.csnlink.controller;

import net.ionoff.csnlink.model.Link;
import net.ionoff.csnlink.service.LinkService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = "/links")
public class LinkController {

	@Autowired
	private LinkService linkService;

	@RequestMapping(value = "",
			method = RequestMethod.GET,
			produces = "application/json")
	@ResponseBody
	public List<Link> get(@RequestParam(name = "status") Link.Status status,
                          @RequestParam(name = "limit", required = false) Integer limit) {
		return linkService.get(status, limit);
	}

	@RequestMapping(value = "/{id}",
			method = RequestMethod.POST,
			produces = "application/json")
	@ResponseBody
	public Link put(@PathVariable(name = "id") Long id,
					@RequestBody Link link) {
		link.setId(id);
		return linkService.save(link);
	}

	@RequestMapping(value = "",
			method = RequestMethod.POST,
			produces = "application/json")
	@ResponseBody
	public List<Link> post(@RequestBody List<String> links){
		return linkService.insert(links);
	}


    @RequestMapping(value = "",
            method = RequestMethod.PUT,
            produces = "application/json")
    @ResponseBody
    public List<Link> put(@RequestBody List<Link> links){
        return linkService.save(links);
    }

}
